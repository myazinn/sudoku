import Dependencies._

ThisBuild / scalaVersion     := "2.13.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.home"
ThisBuild / organizationName := "home"

lazy val root = (project in file("."))
  .settings(
    name := "sudoku",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalaCheck % Test,
    libraryDependencies += scalaTestPlus % Test,
  )
