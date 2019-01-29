import sbt._
import Keys._

val appVersion = "0.4"

val settings: Seq[Def.Setting[_]] = Seq(
  version := appVersion,
  scalaVersion := "2.12.8",
  scalacOptions ++= Seq("-Ypartial-unification", "-feature")
)

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")

lazy val advanced = (project in file("."))
  .settings(settings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-free" % "1.5.0"
      )
  )
