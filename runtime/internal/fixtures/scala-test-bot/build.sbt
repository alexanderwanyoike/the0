ThisBuild / scalaVersion := "3.3.3"
ThisBuild / version := "1.0.0"

lazy val root = (project in file("."))
  .settings(
    name := "scala-test-bot",
    assembly / mainClass := Some("Main"),
    assembly / assemblyJarName := "scala-test-bot-assembly.jar"
  )
