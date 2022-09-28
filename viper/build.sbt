name := "viper"
organization := "vercors"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % "test"

// Disable documentation generation
Compile / doc / sources := Nil
Compile / packageDoc / publishArtifact := false

Test / unmanagedClasspath += Attributed.blank(file(".") / "src" / "main" / "universal" / "deps")
