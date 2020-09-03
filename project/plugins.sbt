addSbtPlugin     ("org.portable-scala"  % "sbt-scalajs-crossproject" % "0.6.1")
addSbtPlugin     ("org.scala-js"        % "sbt-scalajs"              % "0.6.32")

resolvers += Resolver.file("ivy-local", file("ivy-local"))(Resolver.ivyStylePatterns)
