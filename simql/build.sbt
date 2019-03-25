import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / organization     := "me.kerfume"

val jvmDeps = Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
      "org.typelevel" %% "cats-core" % "1.6.0",
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
)

lazy val root = project
  .in(file(".")).settings(
    libraryDependencies ++= jvmDeps
  ).aggregate(simqlJVM, simqlJS)

lazy val simql = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
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
    libraryDependencies ++= jvmDeps
  )

lazy val simqlJS = simql.js
lazy val simqlJVM = simql.jvm
