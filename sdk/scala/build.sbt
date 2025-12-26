ThisBuild / scalaVersion := "3.3.3"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "dev.the0"
ThisBuild / organizationName := "the0"
ThisBuild / organizationHomepage := Some(url("https://the0.dev"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/alexanderwanyoike/the0"),
    "scm:git@github.com:alexanderwanyoike/the0.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id = "alexanderwanyoike",
    name = "Alexander Wanyoike",
    email = "hello@the0.dev",
    url = url("https://github.com/alexanderwanyoike")
  )
)

ThisBuild / description := "the0 trading bot SDK for Scala"
ThisBuild / licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / homepage := Some(url("https://github.com/alexanderwanyoike/the0/tree/main/sdk/scala"))

// Publishing to GitHub Packages
ThisBuild / publishTo := Some("GitHub Package Registry" at "https://maven.pkg.github.com/alexanderwanyoike/the0")
ThisBuild / credentials += Credentials(
  "GitHub Package Registry",
  "maven.pkg.github.com",
  "alexanderwanyoike",
  sys.env.getOrElse("GITHUB_TOKEN", "")
)

lazy val root = (project in file("."))
  .settings(
    name := "the0-sdk",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.18" % Test
    )
  )
