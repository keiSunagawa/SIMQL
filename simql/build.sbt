import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / organization     := "me.kerfume"

lazy val simql = crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
    .in(file("."))
    .settings(
    name := "simql",
    version := "0.1-SNAPSHOT"
  ).jsSettings(
      libraryDependencies ++= Seq(
        "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.1",
        "org.typelevel" %%% "cats-core" % "1.6.0",
        "org.scalatest" %% "scalatest" % "3.0.5" % Test
      )
  ).jvmSettings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
      "org.typelevel" %% "cats-core" % "1.6.0",
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  )

lazy val simqlJS = simql.js
lazy val simqlJVM = simql.jvm
