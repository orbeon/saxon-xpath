import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

enablePlugins(ScalaJSPlugin)

lazy val saxon = (crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full) in file("saxon"))
  .settings(
    organization := "org.orbeon",
    name         := "saxon",
    version      := "1.0-SNAPSHOT",

    scalaVersion := "2.13.1",

    sourceDirectory in ThisProject := baseDirectory.value,

    libraryDependencies += "org.jetbrains" % "annotations"  % "17.0.0",
    libraryDependencies += "com.ibm.icu"   % "icu4j"        % "63.1", // Java  only
    libraryDependencies += "xml-resolver"  % "xml-resolver" % "1.2"   // Java  only
  )

lazy val saxonJS = saxon.js
lazy val saxonJVM = saxon.jvm
