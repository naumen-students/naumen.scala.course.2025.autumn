name := "Lecture2"
version := "1.0.0"
scalaVersion := "2.13.12"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.1" % Test

testFrameworks += new TestFramework("utest.runner.Framework")