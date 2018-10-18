import sbt._
import Keys._

val appVersion = "0.3"

val settings: Seq[Def.Setting[_]] = Seq(
  version := appVersion,
  scalaVersion := "2.12.6",
  scalacOptions ++= Seq("-Ypartial-unification", "-feature")
)

lazy val advanced = (project in file("."))
  .settings(settings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-free" % "1.4.0"
      )
  )
