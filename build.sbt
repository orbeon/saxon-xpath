import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

enablePlugins(ScalaJSPlugin)


val saxonVersion = "10.0.0.27-SNAPSHOT"

val scala212 = "2.12.12"
val scala213 = "2.13.3"
val supportedScalaVersions = List(scala212, scala213)

val ScalaTestVersion = "3.2.1"
val ScalaCollectionCompatVersion  = "2.2.0"

ThisBuild / githubOwner       := "orbeon"
ThisBuild / githubRepository  := "saxon-xpath"
ThisBuild / githubTokenSource := TokenSource.Environment("GITHUB_TOKEN")
ThisBuild / traceLevel        := 0

//ThisBuild / Compile    / publishArtifact := false
//ThisBuild / packageDoc / publishArtifact := false

lazy val DebugTest = config("debug-test") extend Test

lazy val saxon = (crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full) in file("saxon"))
  .settings(
    organization := "org.orbeon",
    name         := "saxon",
    version      := saxonVersion,

    scalaVersion       := scala213,
    crossScalaVersions := supportedScalaVersions,

    scalacOptions ++= Seq(
      "-encoding", "utf8",
      "-deprecation",
      "-unchecked"
    ),

    libraryDependencies += "org.scala-lang.modules"  %%% "scala-collection-compat" % ScalaCollectionCompatVersion,

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
  .jsSettings(
    libraryDependencies ++= Seq("org.xml" %%% "sax"% "2.0.2.2-SNAPSHOT"),
    //  .enablePlugins(TzdbPlugin)
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.0.0",
//    zonesFilter := {(z: String) => z == "America/Los_Angeles"} // Q: See if/how we do this filtering
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.0.0" % Test // for now, get the whole database
  )

lazy val saxonJS  = saxon.js
lazy val saxonJVM = saxon.jvm.configs(DebugTest)

lazy val root = project.in(file("."))
  .aggregate(saxonJS, saxonJVM)
  .settings(
    publish                       := {},
    publishLocal                  := {},
    ThisProject / sourceDirectory := baseDirectory.value / "root",
    crossScalaVersions            := Nil // "crossScalaVersions must be set to Nil on the aggregating project"
  )
