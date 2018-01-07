import sbt._
import Keys._

val appVersion = "0.2"

val settings: Seq[Def.Setting[_]] = Seq(
  version := appVersion,
  scalaVersion := "2.12.4",
  scalacOptions ++= Seq("-Ypartial-unification", "-feature")
)

lazy val advanced = (project in file("."))
  .settings(settings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-free" % "1.0.1"
      )
  )
