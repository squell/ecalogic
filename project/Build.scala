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

object ECALogicBuild extends Build {

  lazy val Distribute = config("distribute").hide

  lazy val launcher = taskKey[File]("Creates a launcher jar.")

  lazy val main = project in file(".") settings (buildSettings: _*) settings (distributeSettings: _*) settings (
    name                               := "ecalogic",
    mainClass in (Compile, packageBin) := Some("nl.ru.cs.ecalogic.ECALogic"),
    mainClass in Distribute            := Some("nl.ru.cs.ecalogic.util.SBTMain"),

    includeFilter in (Distribute, unmanagedResources) := "README.md" || "LICENSE",

    libraryDependencies                += "org.scalatest" %% "scalatest" % "2.0" % "test"
  )
  lazy val web = project in file("web") dependsOn main settings (buildSettings: _*)

  lazy val buildSettings = Seq (
    organization   := "nl.ru.cs.ecalogic",
    version        := "0.1-SNAPSHOT",
    scalaVersion   := "2.10.3",
    exportJars     := true,
    crossPaths     := false,
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-encoding", "UTF8")
  )

  lazy val distributeSettings = inConfig(Distribute)(Seq (
    //target                       := target.value / "dist",
    artifactPath                 := target.value / s"${name.value}-${version.value}.zip",
    mainClass                   <<= mainClass in (Compile, packageBin),

    resourceDirectory           <<= baseDirectory,
    unmanagedResourceDirectories := Seq(resourceDirectory.value),
    resourceDirectories         <<= unmanagedResourceDirectories,
    unmanagedResources          <<= Defaults.collectFiles(unmanagedResourceDirectories, includeFilter in unmanagedResources, excludeFilter in unmanagedResources),
    mappings                    <<= Defaults.resourceMappings,

    includeFilter in unmanagedResources := NothingFilter,

    dependencyClasspath         <<= externalDependencyClasspath,
    externalDependencyClasspath <<= Classpaths.concat(unmanagedClasspath, managedClasspath),
    managedClasspath             := Classpaths.managedJars(configuration.value, classpathTypes.value, update.value),
    unmanagedClasspath          <<= Classpaths.unmanagedDependencies,

    launcher                    <<= (dependencyClasspath, target, name, organization, version, mainClass, streams) map {
      (classpath, target, name, organization, version, mainClass, s) =>
        IO.withTemporaryDirectory { directory =>
          val destJar = target / "sbt-launch.jar"
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
    ivyConfigurations    += Distribute,
    resolvers            += sbtResolver.value,
    libraryDependencies ++= Seq (
      "org.scala-sbt" % "sbt-launch"         % sbtVersion.value % Distribute.name,
      "org.scala-sbt" % "launcher-interface" % sbtVersion.value % "provided"
    )
  )

  private def collectFiles(s: TaskStreams)(f: File, n: Option[String] = None): Iterable[(File, String)] = f match {
    case f if f.isDirectory => f.listFiles.flatMap(f => collectFiles(s)(f, Some(n.fold("")(_ + "/") + f.getName)))
    case f                  => s.log.info(s"  Added ${n.getOrElse(f.getName)}"); Seq(f -> n.getOrElse(f.getName))
  }

  def launcherConfig(name: String, organization: String, version: String, mainClass: Option[String]) =
    s"""[scala]
       |  version: $${sbt.scala.version-auto}
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
       |#  typesafe-ivy-releases: http://repo.typesafe.com/typesafe/ivy-releases/, [organization]/[module]/[revision]/[type]s/[artifact](-[classifier]).[ext], bootOnly
       |  maven-central
       |
       |[boot]
       |  directory: $${ecalogic.boot.directory-$${ecalogic.global.base-$${user.home}/.ecalogic}/boot/}
       |
       |#[ivy]
       |#  ivy-home: $${sbt.ivy.home-$${user.home}/.ivy2/}
       |#  checksums: $${sbt.checksums-sha1,md5}
       |#  override-build-repos: $${sbt.override.build.repos-false}
       |#  repository-config: $${sbt.repository.config-$${sbt.global.base-$${user.home}/.sbt}/repositories}
       |""".stripMargin

}
