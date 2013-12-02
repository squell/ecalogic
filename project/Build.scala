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
import Keys._
import com.typesafe.sbt.SbtProguard._
import ProguardKeys._

object ECALogicBuild extends Build {

  lazy val distributionFiles = TaskKey[Seq[(File, String)]]("distribution-files", "Files and folder for distribution.")
  lazy val distribute = TaskKey[File]("distribute", "Creates a distributable zip file.")

  lazy val main = Project(id = "main", base = file("."), settings = buildProperties)
  lazy val web = Project(id = "web", base = file("web"), dependencies = Seq(main))

  override def settings = super.settings ++ Seq (
    exportJars := true
  )

  val buildProperties = Project.defaultSettings ++ proguardSettings ++ Seq (
    distributionFiles <<= (proguard in Proguard, name) map { (proguardOutput, name) =>
      proguardOutput map {
        case f if f.getName.startsWith(name) => f -> s"lib/$name.jar"
        case f                               => f -> s"lib/${f.getName}"
      }
    },

    distribute <<= (distributionFiles, target, name, version, streams) map {
      (distributionFiles, target, name, version, s) =>

      val zipFile = target / s"$name-$version.zip"
      IO.delete(zipFile)

      s.log.info(s"Building distribution ${zipFile.getAbsolutePath} ...")

      def collectFiles(f: File, n: String): Iterable[(File, String)] = f match {
        case f if f.isDirectory => f.listFiles.flatMap(f => collectFiles(f, s"$n/${f.getName}"))
        case f                  => s.log.info(s"Added $n"); Seq(f -> n)
      }

      val files = distributionFiles.flatMap((collectFiles _).tupled)

      IO.zip(files, zipFile)
      s.log.info("Done building distribution")
      zipFile
    },

    options in Proguard     <++= baseDirectory map (baseDirectory => IO.readLines(baseDirectory / "project" / "proguard.cfg")),
    inputFilter in Proguard <<= (packageBin in Compile) map (artifact => file => if (file == artifact) None else Some("!META-INF/**,!*"))
  )

}
