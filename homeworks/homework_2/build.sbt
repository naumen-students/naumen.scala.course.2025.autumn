name := "Lecture2"

version := "1.10.7"

scalaVersion := "2.13.16"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.9.1" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")