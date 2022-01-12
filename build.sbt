ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

lazy val root = (project in file("."))
  .settings(
    name := "expr-derivation"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0"
