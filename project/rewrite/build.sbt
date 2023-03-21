Compile / sourceDirectory := baseDirectory.value.getParentFile.getParentFile / "src" / "rewrite"
Compile / scalaSource := (Compile / sourceDirectory).value
Compile / resourceDirectory := baseDirectory.value.getParentFile.getParentFile / "res" / "rewrite"
Test / sourceDirectory := baseDirectory.value.getParentFile.getParentFile / "test" / "rewrite"
Test / scalaSource := (Test / sourceDirectory).value

name := "rewrite"
organization := "vercors"

// Disable documentation generation
Compile / doc / sources := Nil
Compile / packageDoc / publishArtifact := false

libraryDependencies += "org.sosy-lab" % "java-smt" % "3.14.3"

// As of 2.0.0, this json library uses LinkedHashMap internally, so the output should be deterministic
// This is important when producing json artefacts based on the AST
libraryDependencies += "com.lihaoyi" %% "upickle" % "2.0.0"
