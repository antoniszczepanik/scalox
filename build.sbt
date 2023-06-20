scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "Scalox",
    version := "0.0.1",
    mainClass := Some("Scalox.scalox")
  )
