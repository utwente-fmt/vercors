enablePlugins(PackPlugin)
enablePlugins(Antlr4Plugin)

name := "Vercors"
organization  := "University of Twente"
version := "0.1-SNAPSHOT"
scalaVersion := "2.11.8"

libraryDependencies += "commons-io" % "commons-io" % "2.4"
libraryDependencies += "com.google.code.gson" % "gson" % "2.8.0"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.4.2" % Test

scalacOptions += "-deprecation"
scalacOptions += "-feature"
scalacOptions += "-unchecked"
scalacOptions += "-Dscalac.patmat.analysisBudget=off"

antlr4PackageName in Antlr4 := Some("vct.antlr4.generated")
antlr4GenVisitor in Antlr4 := true
antlr4Version in Antlr4 := "4.5.3"

// Add dependency to find the Hybrid Runtime Environment.
dependencyClasspath in Compile += new File("../hre/bin")
dependencyClasspath in Compile += new File("../parsers/bin")
dependencyClasspath in Compile += new File("../viper/viper-api/bin")

dependencyClasspath in Test += new File("../hre/bin")

// Make publish-local also create a test artifact, i.e., put a jar-file into the local Ivy
// repository that contains all classes and resources relevant for testing.
// Other projects, e.g., Carbon or Silicon, can then depend on the Sil test artifact, which
// allows them to access the Sil test suite.

publishArtifact in (Test, packageBin) := true

sourceGenerators in Compile += sourceManaged in Compile map { dir =>
  System.err.printf("""generating file""");
  val file = dir / "demo" / "Test.scala"
  IO.write(file, """object Test extends App { println("Hi") }""")
  Seq(file)
}
