ThisBuild / scalaVersion := "2.12.4"
ThisBuild / organization := "ch.overney"

lazy val adventOfCode = (project in file("."))
  .settings(
    name := "Advent Of Code"
  )