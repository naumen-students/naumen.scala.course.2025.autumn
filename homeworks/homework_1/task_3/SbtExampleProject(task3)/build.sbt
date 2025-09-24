ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"

lazy val root = (project in file("."))
  .settings(
    name := "SbtExampleProject"
  )
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"