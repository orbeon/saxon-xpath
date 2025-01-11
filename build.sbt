import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import org.scalajs.linker.interface.ESVersion

enablePlugins(ScalaJSPlugin)

val saxonVersion = "10.0.0.80-SNAPSHOT"

lazy val scala212 = "2.12.19"
lazy val scala213 = "2.13.15"
//val supportedScalaVersions = List(scala212, scala213)
val supportedScalaVersions = List(scala213)

val SaxVersion                    = "2.0.2.9-SNAPSHOT"
val ScalaTestVersion              = "3.2.19"
val ScalaCollectionCompatVersion  = "2.12.0"
val ScalaJsTimeVersion            = "2.6.0"

ThisBuild / githubOwner       := "orbeon"
ThisBuild / githubRepository  := "saxon-xpath"
ThisBuild / githubTokenSource := TokenSource.Environment("GITHUB_TOKEN")
ThisBuild / traceLevel        := 0

//ThisBuild / Compile    / publishArtifact := false
//ThisBuild / packageDoc / publishArtifact := false

sources in (Compile, doc) := Nil
publishArtifact in (Compile, packageDoc) := false

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

    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oF")
  )
  .configs(DebugTest)
  .jvmSettings(
    DebugTest / fork              := true, // "By default, tests executed in a forked JVM are executed sequentially"
    DebugTest / sourceDirectory   := (Test / sourceDirectory).value,
    DebugTest / javaOptions       += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005",
    DebugTest / parallelExecution := false
  )
  .jsSettings(
    scalaJSLinkerConfig ~= (_.withESFeatures(_.withESVersion(ESVersion.ES2018))),
    libraryDependencies ++= Seq("org.xml" %%% "sax" % SaxVersion),
    //  .enablePlugins(TzdbPlugin)
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % ScalaJsTimeVersion,
//    zonesFilter := {(z: String) => z == "America/Los_Angeles"} // Q: See if/how we do this filtering
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time-tzdb" % ScalaJsTimeVersion % Test // for now, get the whole database
  )

lazy val saxonJS  = saxon.js
lazy val saxonJVM = saxon.jvm.configs(DebugTest).settings(
  libraryDependencies += "net.sf.saxon" % "Saxon-HE" % "10.1"
)

lazy val root = project.in(file("."))
  .aggregate(saxonJS, saxonJVM)
  .settings(
    publish                       := {},
    publishLocal                  := {},
    ThisProject / sourceDirectory := baseDirectory.value / "root",
    crossScalaVersions            := Nil // "crossScalaVersions must be set to Nil on the aggregating project"
  )
