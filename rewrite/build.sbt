name := "rewrite"
organization := "vercors"

// Disable documentation generation
Compile / doc / sources := Nil
Compile / packageDoc / publishArtifact := false

libraryDependencies += "org.sosy-lab" % "java-smt" % "3.14.3"
