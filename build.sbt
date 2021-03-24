lazy val root = (project in file(".")).
  settings(
    name := "Riddlers",
    version := "2021.1",
    scalaVersion := "3.0.0-RC1"
  )

libraryDependencies += "org.scalameta" %% "munit" % "0.7.22" % Test
testFrameworks += new TestFramework("munit.Framework")