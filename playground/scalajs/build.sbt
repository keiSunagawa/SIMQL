ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "me.kerfume"

lazy val simqlJS = ProjectRef(file("../../simql"), "simqlJS")

lazy val playground = (project in file("playground"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "playground",
    scalaJSModuleKind := ModuleKind.CommonJSModule,
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "1.6.0",
      "org.typelevel" %%% "cats-free" % "1.6.0",
      "io.monix" %%% "monix" % "3.0.0-RC2",
      "org.scalaz" %%% "scalaz-zio" % "1.0-RC1",
      "org.scalatest" %% "scalatest" % "3.0.5"
    ),
    scalacOptions ++= Seq(
      "-P:scalajs:sjsDefinedByDefault"
    )
  )
  .dependsOn(simqlJS)
