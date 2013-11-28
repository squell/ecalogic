lazy val main = project in file(".")

lazy val web = project dependsOn main

name := "ecalogic"

organization := "nl.ru.cs.ecalogic"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.2" % "test"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-encoding", "UTF8")

mainClass in (Compile, packageBin) := Some("nl.ru.cs.ecalogic.analysis.EnergyAnalysis")
