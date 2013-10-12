name := "ecalogic"

organization := "nl.ru.cs.ecalogic"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.2" % "test"

scalacOptions ++= Seq("-language:implicitConversions", "-language:dynamics", "-deprecation", "-unchecked", "-encoding", "UTF8")
