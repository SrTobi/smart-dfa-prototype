import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

lazy val commonSettings = Seq(
  organization := "de.srtobi",
  scalaVersion := "2.13.1",

  scalacOptions ++= Seq("-deprecation", "-unchecked"),

  libraryDependencies += "org.scalactic" %%% "scalactic" % "3.1.1" % Test,
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.1.1" % Test,
  libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.3.0"
)

lazy val root = project
  .in(file("."))
  .aggregate(cli, web, coreJS, coreJVM)
  .settings(commonSettings)
  .settings(
    name := "dfa-test"
  )

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "dfa-test",
    publish := {},
    publishLocal := {},
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val web = project
  .in(file("web"))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(ScalaJSBundlerPlugin)
  .dependsOn(coreJS)
  .settings(commonSettings)
  .settings(
    scalaJSUseMainModuleInitializer := false,
    copyTask("web/app/src")
  )

lazy val cli = project
  .in(file("cli"))
  .dependsOn(coreJVM)
  .settings(commonSettings)



def copyTask(odir: String) = {
  lazy val copyJSOutput = taskKey[Unit]("copy scala.js linker outputs to another location")
  Seq(
    copyJSOutput := {
      println(s"Copying artifact ${scalaJSLinkedFile.in(Compile).value.data.getAbsolutePath} to [${odir}]")
      val src = file(scalaJSLinkedFile.in(Compile).value.data.getAbsolutePath)
      IO.copy(Seq(
        (src, file(odir) / src.name),
        (file(src.getCanonicalPath + ".map"), file(odir) / (src.name + ".map"))
      ), CopyOptions(overwrite = true, preserveLastModified = true, preserveExecutable = true))
    },
    fastOptJS / copyJSOutput := (copyJSOutput triggeredBy fastOptJS.in(Compile)).value,
    fullOptJS / copyJSOutput := (copyJSOutput triggeredBy fullOptJS.in(Compile)).value
  )
}