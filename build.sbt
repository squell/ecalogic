name := "ecalogic"

organization in ThisBuild := "nl.ru.cs.ecalogic"

version in ThisBuild := "0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.10.3"

scalacOptions in ThisBuild ++= Seq("-deprecation", "-unchecked", "-encoding", "UTF8")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

mainClass in (Compile, packageBin) := Some("nl.ru.cs.ecalogic.ECALogic")

distributionFiles += file("components/") -> "components"
