/*
 * ecalogic: a tool for performing energy consumption analysis.
 *
 * Copyright (c) 2013, J. Neutelings, D. Peelen, M. Schoolderman
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *   Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 *   Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 *
 *   Neither the name of the Radboud University Nijmegen nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

import org.apache.commons.compress.archivers.tar.{TarArchiveOutputStream, TarArchiveEntry}
import org.apache.commons.compress.archivers.{ArchiveOutputStream, ArchiveEntry}
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream
import sbt._
import sbt.Keys._

import com.typesafe.sbt.SbtProguard._
import ProguardKeys._

//import com.earldouglas.xsbtwebplugin.WebPlugin

import java.io.{FileOutputStream, OutputStream, FileInputStream}
import org.apache.commons.compress.archivers.zip.{ZipArchiveEntry, ZipArchiveOutputStream}

object Build extends sbt.Build {

  private val FileAccessNormal  = Integer.parseInt("644", 8)
  private val FileAccessExecute = Integer.parseInt("755", 8)

  lazy val standalone                    = taskKey[File]("Creates a stand-alone jar.")
  lazy val distribute                    = taskKey[Seq[File]]("Creates distributable archives.")
  lazy val distributionResourceDirectory = settingKey[File]("Directory which contains distributable files.")

  override lazy val settings = super.settings ++ Seq (
    organization   := "nl.ru.cs.ecalogic",
    version        := "0.1a-SNAPSHOT",
    scalaVersion   := "2.10.3",
    crossPaths     := false,
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-encoding", "UTF8")
  )

  lazy val main = project in file(".") aggregate LocalProject("web") settings (proguardSettings: _*) settings (
    name                               := "ecalogic",
    mainClass in (Compile, packageBin) := Some("nl.ru.cs.ecalogic.ECALogic"),
    libraryDependencies                += "org.scalatest" %% "scalatest" % "2.0" % "test",
    exportJars                         := true,

    distributionResourceDirectory      := (sourceDirectory in Compile).value / "distributable",

    standalone                        <<= proguard in Proguard map (_.head),

    outputs in Proguard               <<= (artifactPath in (Compile, packageBin)) map { path =>
      val name = path.getName
      Seq(path.getParentFile / s"${name.substring(0, name.lastIndexOf('.'))}-standalone.jar")
    },
    options in Proguard              <++= baseDirectory map (baseDirectory => IO.readLines(baseDirectory / "project" / "proguard.cfg")),
    inputFilter in Proguard           <<= (packageBin in Compile) map (artifact => file => if (file == artifact) None else Some("!META-INF/**,!*")),

    distribute                        <<= (artifactPath in (Compile, packageBin), standalone, distributionResourceDirectory, streams) map {
      (artifactPath, standalone, distDirectory, s) =>
        val resources = PathFinder(distDirectory).descendantsExcept(AllPassFilter, NothingFilter)
          .filter(!_.isDirectory).pair(Path.relativeTo(distDirectory), false) map {
          case (from, to) => from -> to.replace('\\', '/')
        }
        val files = Seq(standalone -> "bin/ecalogic.jar") ++ resources

        s.log.info("Building distributable zip file ...")
        val zipFile = artifactPath.getParentFile / artifactPath.getName.replace(".jar", ".zip")
        val zip = new ZipArchiveOutputStream(zipFile)
        try {
          files.foreach { case (from, to) =>
            val entry = new ZipArchiveEntry(from, to)
            entry.setUnixMode(if (to == "bin/ecalogic") FileAccessExecute else FileAccessNormal)
            addEntry(entry, from, zip, s)
          }
        } finally {
          zip.close()
        }
        s.log.info("Done.")

        s.log.info("Building distributable tar file ...")
        val tarFile = artifactPath.getParentFile / artifactPath.getName.replace(".jar", ".tar.gz")
        val tar = new TarArchiveOutputStream(new GzipCompressorOutputStream(new FileOutputStream(tarFile)))
        try {
          files.foreach { case (from, to) =>
            val entry = new TarArchiveEntry(from, to)
            entry.setMode(if (to == "bin/ecalogic") FileAccessExecute else FileAccessNormal)
            addEntry(entry, from, tar, s)
          }
        } finally {
          tar.close()
        }
        s.log.info("Done.")

        Seq(zipFile, tarFile)
    }
  )

  lazy val web = project dependsOn main settings (
  //lazy val web = project dependsOn main settings (WebPlugin.webSettings: _*) settings (
    name                 := "ecalogic-webapp",
    libraryDependencies ++= {
      val liftVersion = "2.5.1"
      val lv = "2.5"
      Seq (
        "net.liftweb"       %% "lift-webkit"             % liftVersion,             // Required for Lift
        "net.liftmodules"   %% s"lift-jquery-module_$lv" % "2.5",                   // Required for JQuery
        "ch.qos.logback"    %  "logback-classic"         % "1.0.13"/*,                // Required to log messages

        "org.eclipse.jetty" %  "jetty-webapp"            % "9.1.0.+" % "container", // Required for web-plugin
        "org.eclipse.jetty" %  "jetty-plus"              % "9.1.0.+" % "container"  // Required for web-plugin
        */
      )
    },
    distribute           <<= (Keys.`package` in Compile) map (Seq(_))
    // unmanagedResourceDirectories in Test <++= PluginKeys.webappResources in Compile
  )

  def addEntry(entry: ArchiveEntry, file: File, archive: ArchiveOutputStream, s: TaskStreams) {
    archive.putArchiveEntry(entry)

    val stream = new FileInputStream(file)
    try {
      val buf = Array.ofDim[Byte](8192)
      var len = stream.read(buf)
      while (len > 0) {
        archive.write(buf, 0, len)
        len = stream.read(buf)
      }
    } finally {
      stream.close()
    }

    archive.closeArchiveEntry()
    s.log.info(s"  Added ${entry.getName}")
  }

}
