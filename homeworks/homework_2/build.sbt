name := "Lecture2"

version := "0.1"

scalaVersion := "2.12.18"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.2" % Test
testFrameworks += new TestFramework("utest.runner.Framework")