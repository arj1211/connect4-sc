import Dependencies._

ThisBuild / scalaVersion := "2.13.16"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.connect4"
ThisBuild / organizationName := "connect4"

lazy val root = (project in file("."))
  .settings(
    name := "connect4",
    libraryDependencies += munit % Test
  )
