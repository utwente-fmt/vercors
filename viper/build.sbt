Compile / sourceDirectory := baseDirectory.value / ".." / "src" / "viper"
Compile / scalaSource := (Compile / sourceDirectory).value
Test / sourceDirectory := baseDirectory.value / ".." / "test" / "viper"
Test / scalaSource := (Test / sourceDirectory).value

name := "viper"
organization := "vercors"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % "test"

// Disable documentation generation
Compile / doc / sources := Nil
Compile / packageDoc / publishArtifact := false

Test / unmanagedClasspath += Attributed.blank(file(".") / "src" / "main" / "universal" / "deps")
