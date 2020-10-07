import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

enablePlugins(ScalaJSPlugin)


lazy val scala212 = "2.12.12"
lazy val scala213 = "2.13.3"
lazy val supportedScalaVersions = List(scala212, scala213)

val ScalaTestVersion = "3.2.1"

traceLevel in ThisBuild := 0

lazy val DebugTest = config("debug-test") extend Test

lazy val saxon = (crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full) in file("saxon"))
  .settings(
    organization := "org.orbeon",
    name         := "saxon",
    version      := "1.0-SNAPSHOT",

    scalaVersion := scala213,

    scalacOptions ++= Seq(
    "-encoding", "utf8",
    "-deprecation",
    "-unchecked"
    ),

    libraryDependencies += "org.scalactic" %%% "scalactic"     % ScalaTestVersion    % Test,
    libraryDependencies += "org.scalatest" %%% "scalatest"     % ScalaTestVersion    % Test,

    libraryDependencies += "org.jetbrains" %   "annotations"   % "17.0.0",
    libraryDependencies += "com.ibm.icu"   %   "icu4j"         % "63.1", // Java  only
    libraryDependencies += "xml-resolver"  %   "xml-resolver"  % "1.2",  // Java  only

    testOptions       in Test          += Tests.Argument(TestFrameworks.ScalaTest, "-oF")
  )
  .configs(DebugTest)
  .jvmSettings(
    fork              in DebugTest     := true, // "By default, tests executed in a forked JVM are executed sequentially"
    sourceDirectory   in DebugTest     := (sourceDirectory in Test).value,
    javaOptions       in DebugTest     += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005",
    parallelExecution in DebugTest     := false
  )

lazy val saxonJS  = saxon.js
lazy val saxonJVM = saxon.jvm.configs(DebugTest)
