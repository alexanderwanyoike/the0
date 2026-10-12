ThisBuild / scalaVersion := "3.3.3"
ThisBuild / version := "0.4.0"
ThisBuild / organization := "dev.the0"
ThisBuild / organizationName := "the0"
ThisBuild / organizationHomepage := Some(url("https://the0.app"))

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
    email = "hello@the0.app",
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
      "org.scalatest" %% "scalatest" % "3.2.18" % Test,
      "com.lihaoyi" %% "ujson" % "4.0.2" % Test
    ),
    // ContractSpec starts each contract case in a JVM of its own, which needs the test
    // classpath; javaOptions only reach the tests when they run in a forked JVM.
    Test / fork := true,
    Test / javaOptions ++= Seq(
      s"-Dthe0.contract.classpath=${(Test / fullClasspath).value.files.mkString(java.io.File.pathSeparator)}",
      s"-Dthe0.contract.cases=${(baseDirectory.value / ".." / "contract" / "cases").getCanonicalPath}"
    )
  )
