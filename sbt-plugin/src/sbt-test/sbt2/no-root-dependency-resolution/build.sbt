ThisBuild / version := scalaJSVersion
ThisBuild / scalaVersion := "3.3.4"

// Root project - skip compilation to avoid output directory conflicts
lazy val root = (project in file("."))
  .aggregate(`my-project`)
  .settings(
    name := "root",
    publish / skip := true
  )

lazy val `my-project` = project
  .enablePlugins(ScalaJSPlugin)
  .settings(scalaJSUseMainModuleInitializer := true)
