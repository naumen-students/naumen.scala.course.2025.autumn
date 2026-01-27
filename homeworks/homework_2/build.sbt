name := "Lecture2"

version := "0.1"

scalaVersion := "3.7.3"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.9.1" % Test

testFrameworks += new TestFramework("utest.runner.Framework")