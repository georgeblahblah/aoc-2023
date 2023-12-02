ThisBuild / scalaVersion := "3.3.1"

lazy val commonSettings = Seq(
    version := "0.1.0-SNAPSHOT",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )

lazy val day1 = (project in file("day1"))
  .settings(commonSettings)
