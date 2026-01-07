import Dependencies._

ThisBuild / scalaVersion := "2.13.18"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "com.connect4"
ThisBuild / organizationName := "connect4"

lazy val core = (project in file("core"))
  .settings(
    name := "connect4-core",
    libraryDependencies ++= Seq(
      munit % Test,
      scalacheck % Test,
      munitScalacheck % Test
    )
  )

lazy val cli = (project in file("cli"))
  .settings(
    name := "connect4-cli",
    Compile / mainClass := Some("connect4.CliApp")
  )
  .dependsOn(core)

lazy val root = (project in file("."))
  .aggregate(cli)
  .settings(
    name := "connect4",
    addCommandAlias("run", "cli/run")
  )
