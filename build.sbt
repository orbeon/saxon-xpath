import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

enablePlugins(ScalaJSPlugin)

val ScalaTestVersion = "3.2.1"

traceLevel in ThisBuild := 0

lazy val saxon = (crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full) in file("saxon"))
  .settings(
    organization := "org.orbeon",
    name         := "saxon",
    version      := "1.0-SNAPSHOT",

    scalaVersion := "2.13.1", // 2.13.3 is not supported with Scala.js 0.6.x

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

lazy val saxonJS  = saxon.js
lazy val saxonJVM = saxon.jvm
