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

import com.earldouglas.xsbtwebplugin.WebPlugin

object ECALogicBuild extends Build {

  lazy val Launcher = config("launcher").hide

  lazy val launcher   = taskKey[File]("Creates a launcher jar.")
  lazy val standalone = taskKey[File]("Creates a stand-alone jar.")

  override lazy val settings = super.settings ++ Seq (
    organization   := "nl.ru.cs.ecalogic",
    version        := "0.1-SNAPSHOT",
    scalaVersion   := "2.10.3",
    crossPaths     := false,
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-encoding", "UTF8"),
    exportJars     := true
  )

  lazy val main = project in file(".") aggregate LocalProject("web") settings (proguardSettings: _*) settings (launcherSettings: _*) settings (
    name                               := "ecalogic",
    mainClass in (Compile, packageBin) := Some("nl.ru.cs.ecalogic.ECALogic"),
    mainClass in Launcher              := Some("nl.ru.cs.ecalogic.util.SBTMain"),
    libraryDependencies                += "org.scalatest" %% "scalatest" % "2.0" % "test",

    mappings in Launcher              ++= Seq (
      baseDirectory.value / "LICENSE"   -> "LICENSE",
      baseDirectory.value / "README.md" -> "README.md"
    ),

    standalone                        <<= proguard in Proguard map (_.head),
    aggregate in standalone            := false,

    outputs in Proguard               <<= (artifactPath in (Compile, packageBin)) map { path =>
      val name = path.getName
      Seq(path.getParentFile / s"${name.substring(0, name.lastIndexOf('.'))}-standalone.jar")
    },
    options in Proguard              <++= baseDirectory map (baseDirectory => IO.readLines(baseDirectory / "project" / "proguard.cfg")),
    inputFilter in Proguard           <<= (packageBin in Compile) map (artifact => file => if (file == artifact) None else Some("!META-INF/**,!*"))
  )

  lazy val web = project dependsOn main settings (WebPlugin.webSettings: _*) settings (
    name                 := "ecalogic-webapp",
    libraryDependencies ++= {
      val liftVersion = "2.5.1"
      val lv = "2.5"
      Seq (
        "net.liftweb"       %% "lift-webkit"             % liftVersion,             // Required for Lift
        "net.liftmodules"   %% s"lift-jquery-module_$lv" % "2.5",                   // Required for JQuery
        "ch.qos.logback"    %  "logback-classic"         % "1.0.13",                // Required to log messages

        "org.eclipse.jetty" %  "jetty-webapp"            % "9.1.0.+" % "container", // Required for web-plugin
        "org.eclipse.jetty" %  "jetty-plus"              % "9.1.0.+" % "container"  // Required for web-plugin
      )
    }
    // unmanagedResourceDirectories in Test <++= PluginKeys.webappResources in Compile
  )

  // Settings for creating an sbt-launch configuration file and zip
  lazy val launcherSettings = inConfig(Launcher)(Seq (
    artifactPath                 := target.value / s"${name.value}-launcher.zip",
    mainClass                   <<= mainClass in (Compile, packageBin),

    resourceDirectory           <<= (sourceDirectory in Compile) (_ / "launcher"),
    unmanagedResourceDirectories := Seq(resourceDirectory.value),
    resourceDirectories         <<= unmanagedResourceDirectories,
    unmanagedResources          <<= Defaults.collectFiles(unmanagedResourceDirectories, includeFilter in unmanagedResources, excludeFilter in unmanagedResources),
    mappings                    <<= Defaults.resourceMappings,

    dependencyClasspath         <<= externalDependencyClasspath,
    externalDependencyClasspath <<= Classpaths.concat(unmanagedClasspath, managedClasspath),
    managedClasspath             := Classpaths.managedJars(configuration.value, classpathTypes.value, update.value),
    unmanagedClasspath          <<= Classpaths.unmanagedDependencies,

    launcher                    <<= (dependencyClasspath, target, name, organization, version, mainClass, streams) map {
      (classpath, target, name, organization, version, mainClass, s) =>
        IO.withTemporaryDirectory { directory =>
          val destJar = target / "dist" / "sbt-launch.jar"
          classpath.map(_.data).foreach {
            case f if f.isDirectory => IO.copyDirectory(f, directory)
            case f                  => IO.unzip(f, directory)
          }
          val bootFile = directory / "sbt" / "sbt.boot.properties"
          IO.write(bootFile, launcherConfig(name, organization, version, mainClass))
          s.log.info(s"Building launcher ${destJar.getAbsolutePath} ...")
          IO.zip(collectFiles(s)(directory), destJar)
          s.log.info("Done building launcher")
          destJar
        }
    },

    Keys.`package`              <<= (mappings, launcher, artifactPath, streams) map {
      (mappings, launcherJar, artifactPath, s) =>
        IO.delete(artifactPath)
        s.log.info(s"Building distribution zip ${artifactPath.getAbsolutePath} ...")
        IO.zip(mappings ++ Seq(launcherJar -> "lib/sbt-launch.jar"), artifactPath)
        s.log.info("Done building distribution zip")
        artifactPath
    }
  )) ++ Seq (
    ivyConfigurations    += Launcher,
    resolvers            += sbtResolver.value,

    libraryDependencies ++= Seq (
      "org.scala-sbt" % "launcher-interface" % sbtVersion.value % "provided",
      "org.scala-sbt" % "sbt-launch"         % sbtVersion.value % Launcher.name
    )
  )

  private def collectFiles(s: TaskStreams)(f: File, n: Option[String] = None): Iterable[(File, String)] = f match {
    case f if f.isDirectory => f.listFiles.flatMap(f => collectFiles(s)(f, Some(n.fold("")(_ + "/") + f.getName)))
    case f                  => s.log.info(s"  Added ${n.getOrElse(f.getName)}"); Seq(f -> n.getOrElse(f.getName))
  }

  def launcherConfig(name: String, organization: String, version: String, mainClass: Option[String]) =
    s"""[scala]
       |  version: auto
       |
       |[app]
       |  org: $organization
       |  name: $name
       |  version: $${ecalogic.version-read(ecalogic.version)[$version]}
       |  class: $${ecalogic.main.class-${mainClass.mkString}}
       |  cross-versioned: false
       |
       |[repositories]
       |  local
       |#  maven-central
       |
       |[boot]
       |  directory: $${ecalogic.home-$${user.home}/.ecalogic}/boot/
       |""".stripMargin

}
