import sbt.Keys.name
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

ThisBuild / scalaVersion := "2.12.8"
ThisBuild / organization := "me.kerfume"

val simqlVersion = "0.1-SNAPSHOT"

val jvmDeps = Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
  "org.typelevel" %% "cats-core" % "1.6.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test
)

lazy val root = project
  .in(file("."))
  .settings(
    libraryDependencies ++= jvmDeps
  )
  .aggregate(simql.js, simql.jvm, core.js, core.jvm, query.js, query.jvm, definition.js, definition.jvm)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "core",
    version := simqlVersion
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.1",
      "org.typelevel" %%% "cats-core" % "1.6.0",
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  )
  .jvmSettings(
    libraryDependencies ++= jvmDeps
  )

lazy val query = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("query"))
  .settings(
    name := "query",
    version := simqlVersion
  )
  .jsSettings()
  .jsConfigure(_.dependsOn(core.js))
  .jvmSettings(
    libraryDependencies ++= jvmDeps
  )
  .jvmConfigure(_.dependsOn(core.jvm))

lazy val definition = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("definition"))
  .settings(
    name := "definition",
    version := simqlVersion
  )
  .jsSettings()
  .jsConfigure(_.dependsOn(core.js, query.js))
  .jvmSettings(
    libraryDependencies ++= jvmDeps
  )
  .jvmConfigure(_.dependsOn(core.jvm, query.jvm))

lazy val simql = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("simql"))
  .settings(
    name := "simql",
    version := simqlVersion
  )
  .jsSettings()
  .jsConfigure(_.dependsOn(query.js, definition.js))
  .jvmSettings(
    libraryDependencies ++= jvmDeps
  )
  .jvmConfigure(_.dependsOn(query.jvm, definition.jvm))

lazy val simqlJS = simql.js
lazy val simqlJVM = simql.jvm
