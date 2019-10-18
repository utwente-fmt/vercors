import NativePackagerHelper._
ThisBuild / turbo := true

enablePlugins(PackPlugin)
enablePlugins(JavaAppPackaging)

lazy val viper_api = (project in file("viper"))
lazy val parsers = (project in file("parsers"))

lazy val vercors = (project in file("."))
  .dependsOn(viper_api)
  .dependsOn(parsers)
  .settings(
    name := "Vercors",
    organization := "University of Twente",
    version := "1.0.0",
    scalaVersion := "2.12.7",

    libraryDependencies += "commons-io" % "commons-io" % "2.4",
    libraryDependencies += "com.google.code.gson" % "gson" % "2.8.0",
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.4.2" % Test,

    scalacOptions += "-deprecation",
    scalacOptions += "-feature",
    scalacOptions += "-unchecked",
    scalacOptions += "-Dscalac.patmat.analysisBudget=off",
    
    // Disable documentation generation
    sources in (Compile, doc) := Seq(),

    mappings in Universal += file("../README.md") -> "README.md",
    mappings in Universal ++= directory("../examples"),

    sources in doc in Compile := List(),

    discoveredMainClasses in Compile := Seq(),
    mainClass in Compile := Some("vct.main.Main"),

    // Make publish-local also create a test artifact, i.e., put a jar-file into the local Ivy
    // repository that contains all classes and resources relevant for testing.
    // Other projects, e.g., Carbon or Silicon, can then depend on the Sil test artifact, which
    // allows them to access the Sil test suite.
    publishArtifact in(Test, packageBin) := true,

    mainClass in (Compile, run) := Some("vct.main.Main"),
    assembly / mainClass := Some("vct.main.Main"),    // Define JAR's entry point
    assemblyMergeStrategy in assembly := {
      case "logback.xml" => MergeStrategy.first
      case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
        oldStrategy(x)
    }
  )
