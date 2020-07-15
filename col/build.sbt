name := "col"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1"
// Disable documentation generation
sources in (Compile, doc) := Seq()
publishArtifact in (Compile, packageDoc) := false
publishArtifact in packageDoc := false
