name := "sma-bot"
version := "1.0.1"
scalaVersion := "3.3.1"

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client3" %% "core" % "3.9.0",
  "io.circe" %% "circe-core" % "0.14.6",
  "io.circe" %% "circe-parser" % "0.14.6",
  "io.circe" %% "circe-generic" % "0.14.6"
)

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

assembly / mainClass := Some("Main")
