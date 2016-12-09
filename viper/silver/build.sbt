
name := "silver"

organization  := "viper"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1"

libraryDependencies += "org.rogach" %% "scallop" % "0.9.5"

libraryDependencies += "org.jgrapht" % "jgrapht-core" % "0.9.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

libraryDependencies += "commons-io" % "commons-io" % "2.4"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.3.7"
libraryDependencies +=  "com.google.guava" % "guava" % "17.0"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

scalacOptions += "-unchecked"

scalacOptions += "-Dscalac.patmat.analysisBudget=off"

// Add dependency to find the VerCors/Viper interface.
dependencyClasspath in Compile += new File("../viper-api/bin")

// Make publish-local also create a test artifact, i.e., put a jar-file into the local Ivy
// repository that contains all classes and resources relevant for testing.
// Other projects, e.g., Carbon or Silicon, can then depend on the Sil test artifact, which
// allows them to access the Sil test suite.

publishArtifact in (Test, packageBin) := true

