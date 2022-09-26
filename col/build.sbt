name := "col"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1"

// Disable documentation generation
Compile / doc / sources := Seq()
Compile / packageDoc / publishArtifact := false
