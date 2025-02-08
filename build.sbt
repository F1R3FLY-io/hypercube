name := "hypercube"

version := "0.1.0"
scalaVersion := "3.4.1"

// Include the Scala parser combinators library and ScalaTest for testing.
libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
)

// Additional compiler options for better warnings and encoding.
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-encoding", "utf8"
)