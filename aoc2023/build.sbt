val scala3Version = "3.5.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Advent of code 2023",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq("org.scalameta" %% "munit" % "1.0.0" % Test,
    "com.softwaremill.sttp.client4" %% "core" % "4.0.0-M6",
    "org.scala-lang" %% "toolkit" % "0.1.7",
    )
    
  )
