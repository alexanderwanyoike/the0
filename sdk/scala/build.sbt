ThisBuild / scalaVersion := "3.3.3"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "dev.the0"

lazy val root = (project in file("."))
  .settings(
    name := "the0-scala-sdk",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.18" % Test
    )
  )
