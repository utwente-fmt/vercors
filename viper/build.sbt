name := "viper-api"
organization := "vercors"
version := "1.0-SNAPSHOT"

// Disable documentation generation
sources in (Compile, doc) := Seq()
publishArtifact in (Compile, packageDoc) := false
