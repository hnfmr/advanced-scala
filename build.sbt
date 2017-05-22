import sbt._
import Keys._
import Def._

val appVersion = "0.1"

val settings: Seq[Def.Setting[_]] = Seq(
  version := appVersion,
  scalaVersion := "2.12.2"
)

lazy val advanced = (project in file("."))
  .settings(settings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" % "cats-core_2.12" % "0.9.0"
      )
  )


