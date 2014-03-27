name := """hindleymilner"""

version := "1.0"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest"                 % "1.9.1" % "test",
  "org.scalaz"    %% "scalaz-core"               % "7.0.5",
  "org.scalaz"    %% "scalaz-effect"             % "7.0.5",
  "org.scalaz"    %% "scalaz-typelevel"          % "7.0.5",
  "org.scalaz"    %% "scalaz-scalacheck-binding" % "7.0.5" % "test"
)

scalacOptions := List("-feature")