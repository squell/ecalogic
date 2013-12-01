lazy val main = project in file(".")

lazy val web = project dependsOn main

val MainClass = "nl.ru.cs.ecalogic.ECALogic"

name := "ecalogic"

organization := "nl.ru.cs.ecalogic"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

unmanagedClasspath in Runtime += baseDirectory.value / "components"

unmanagedClasspath in Test += baseDirectory.value / "components"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-encoding", "UTF8")

mainClass in (Compile, packageBin) := Some(MainClass)

proguardSettings

ProguardKeys.options in Proguard ++= IO.readLines(baseDirectory.value / "project" / "proguard.cfg")

ProguardKeys.inputs in Proguard := (ProguardKeys.inputs in Proguard).value filterNot { _.getName == "components" }
