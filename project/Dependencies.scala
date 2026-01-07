import sbt._

object Dependencies {
  val munitVersion = "1.0.0-M10"
  val scalacheckVersion = "1.19.0"
  val munit = "org.scalameta" %% "munit" % munitVersion
  val scalacheck = "org.scalacheck" %% "scalacheck" % scalacheckVersion
  val munitScalacheck = "org.scalameta" %% "munit-scalacheck" % munitVersion
}
