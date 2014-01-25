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

import sbt._
import sbt.Keys._

import com.typesafe.sbt.SbtProguard._
import ProguardKeys._

//import com.earldouglas.xsbtwebplugin.WebPlugin

object Build extends sbt.Build {

  lazy val standalone                    = taskKey[File]("Creates a stand-alone jar.")
  lazy val distribute                    = taskKey[File]("Creates a distributable zip.")
  lazy val distributionResourceDirectory = settingKey[File]("Directory which contains distributable files.")

  override lazy val settings = super.settings ++ Seq (
    organization   := "nl.ru.cs.ecalogic",
    version        := "0.1-SNAPSHOT",
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
        def collectFiles(s: TaskStreams)(f: File, n: Option[String] = None): Iterable[(File, String)] = f match {
          case f if f.isDirectory => f.listFiles.flatMap(f => collectFiles(s)(f, Some(n.fold("")(_ + "/") + f.getName)))
          case f                  => Seq(f -> n.getOrElse(f.getName))
        }

        s.log.info("Building distributable zip file ...")
        val targetFile = artifactPath.getParentFile / artifactPath.getName.replace(".jar", ".zip")
        val files = Seq(standalone -> "lib/ecalogic.jar") ++ collectFiles(s)(distDirectory)
        files.foreach(f => s.log.info(s"  Added ${f._2}"))
        IO.zip(files, targetFile)
        s.log.info("Done.")

        targetFile
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
    distribute          <<= Keys.`package` in Compile
    // unmanagedResourceDirectories in Test <++= PluginKeys.webappResources in Compile
  )

}
