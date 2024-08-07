ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.2"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
libraryDependencies += "io.spray" %% "spray-json" % "1.3.6"

lazy val root = (project in file("."))
  .settings(
    name := "weeks"
  )
