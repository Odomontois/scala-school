import _root_.sbt.Keys._

name := "scala"

version := "1.0"

scalaVersion := "2.12.3"

scalacOptions := Seq(
  "-encoding", "utf8",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-target:jvm-1.8",
  "-Ymacro-debug-lite",
  "-language:_",
  "-Xexperimental")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "org.mockito" % "mockito-core" % "1.9.5" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.4"

libraryDependencies += "com.typesafe.akka" %% "akka-typed" % "2.5.4"

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.5.4"

libraryDependencies += "com.typesafe.akka" %% "akka-slf4j" % "2.5.4"

libraryDependencies +=  "com.typesafe.akka" %% "akka-testkit" % "2.5.4" % "test"