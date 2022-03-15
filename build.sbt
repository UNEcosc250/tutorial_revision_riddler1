lazy val root = (project in file(".")).
  settings(
    name := "Riddlers",
    version := "2022.1",
    scalaVersion := "3.1.0"
  )

libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
testFrameworks += new TestFramework("munit.Framework")
