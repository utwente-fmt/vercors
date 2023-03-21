Compile / sourceDirectory := baseDirectory.value.getParentFile.getParentFile / "src" / "viper"
Compile / scalaSource := (Compile / sourceDirectory).value
Compile / resourceDirectory := baseDirectory.value.getParentFile.getParentFile / "res" / "viper"
Test / sourceDirectory := baseDirectory.value.getParentFile.getParentFile / "test" / "viper"
Test / scalaSource := (Test / sourceDirectory).value

name := "viper"
organization := "vercors"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % "test"

// Disable documentation generation
Compile / doc / sources := Nil
Compile / packageDoc / publishArtifact := false

Test / unmanagedClasspath += Attributed.blank(file(".") / "src" / "main" / "universal" / "deps")
