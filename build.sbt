name := "ecalogic"

organization := "nl.ru.cs.ecalogic"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.10.2"

resolvers += "mtgto repos" at "http://scala-irc-bot.github.com/scala-irc-bot/maven/"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.2" % "test"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-encoding", "UTF8")