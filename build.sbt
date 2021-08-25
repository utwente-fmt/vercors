import NativePackagerHelper._
import sys.process._
import java.io.File.pathSeparator
import java.nio.file.{Files, Path, Paths}
import java.net.URL
import java.util.Comparator
import sbt.internal._


ThisBuild / turbo := true // en wat is daar het praktisch nut van?
ThisBuild / scalaVersion := "2.13.5"

enablePlugins(BuildInfoPlugin)
enablePlugins(JavaAppPackaging)
enablePlugins(DebianPlugin)

/* To update viper, replace the hash with the commit hash that you want to point to. It's a good idea to ask people to
 re-import the project into their IDE, as the location of the viper projects below will change. */
val silver_url = uri("git:https://github.com/viperproject/silver.git#v.21.01-release")
val carbon_url = uri("git:https://github.com/viperproject/carbon.git#v.21.01-release")
val silicon_url = uri("git:https://github.com/viperproject/silicon.git#v.21.01-release")

/*
buildDepdendencies.classpath contains the mapping from project to a list of its dependencies. The viper projects silver,
silicon and carbon specify their dependencies as a regular sbt subproject: they expect a symlink in the project root to
the relevant project. Instead, we replace those dependencies by a reference to the repository as above. So e.g.
"the silver project at hg:carbon" becomes "the silver project at hg:silver". All other dependencies are left alone.
 */
buildDependencies in Global := {
  val log = sLog.value
  val oldDeps = (buildDependencies in Global).value
  def fixDep(dep: ClasspathDep[ProjectRef]): ClasspathDep[ProjectRef] = dep.project.project match {
    case "silver" =>
      ResolvedClasspathDependency(ProjectRef(silver_url, "silver"), dep.configuration)
    case "silicon" =>
      ResolvedClasspathDependency(ProjectRef(silicon_url, "silicon"), dep.configuration)
    case "carbon" =>
      ResolvedClasspathDependency(ProjectRef(carbon_url, "carbon"), dep.configuration)
    case _ =>
      dep
  }
  val newDeps = for((proj, deps) <- oldDeps.classpath) yield (proj, deps map fixDep)
  BuildDependencies(newDeps, oldDeps.aggregate)
}

lazy val silver_ref = ProjectRef(silver_url, "silver")
lazy val carbon_ref = ProjectRef(carbon_url, "carbon")
lazy val silicon_ref = ProjectRef(silicon_url, "silicon")
lazy val hre = project in file("hre")
lazy val col = (project in file("col")).dependsOn(hre)
lazy val parsers = (project in file("parsers")).dependsOn(hre, col)
lazy val viper_api = (project in file("viper")).dependsOn(hre, col, silver_ref, carbon_ref, silicon_ref)

// We fix the scalaVersion of all viper components to be silver's scalaVersion, because
// it seems that in some cases the scalaVersion of the other components is lost.
// SBT then assumes the version we want for those components is 2.10, and then
// suddenly it can't find the dependencies anymore! Smart move, sbt.
// If Viper ever moves to maven central or some other proper dependency mechanism,
// this can probably be removed.
carbon_ref / scalaVersion := (silver_ref / scalaVersion).value
silicon_ref / scalaVersion := (silver_ref / scalaVersion).value
ProjectRef(silver_url, "common") / scalaVersion := (silver_ref / scalaVersion).value
ProjectRef(carbon_url, "common") / scalaVersion := (silver_ref / scalaVersion).value
ProjectRef(silicon_url, "common") / scalaVersion := (silver_ref / scalaVersion).value

// Disable doc generation in all viper projects
carbon_ref / packageDoc / publishArtifact := false
silver_ref / packageDoc / publishArtifact := false
silicon_ref / packageDoc / publishArtifact := false
ProjectRef(silver_url, "common") / packageDoc / publishArtifact := false
ProjectRef(carbon_url, "common") / packageDoc / publishArtifact := false
ProjectRef(silicon_url, "common") / packageDoc / publishArtifact := false

lazy val printMainClasspath = taskKey[Unit]("Prints classpath of main vercors executable")

lazy val vercors: Project = (project in file("."))
  .dependsOn(hre, col, viper_api, parsers)
  .aggregate(hre, col, viper_api, parsers)
  .settings(
    name := "Vercors",
    organization := "University of Twente",
    version := "1.4.0-SNAPSHOT",
    maintainer := "VerCors Team <vercors@lists.utwente.nl>",
    packageSummary := "A tool for static verification of parallel programs",
    packageDescription :=
      """VerCors is a tool for static verification of parallel programs. VerCors aims to verify
        |many different concurrency constructs, including: heterogeneous concurrency (Java and C), GPU kernels
        |using barriers and atomics (OpenCL), and compiler directives as used in deterministic parallelism
        |(OpenMP). VerCors is able to prove data-race freedom, memory safety, and functional correctness of
        |(concurrent) programs written in Java, C, OpenCL, OpenMP, and its own Prototypal Verification Language
        |PVL. """.stripMargin.replaceAll("\n", ""),

    libraryDependencies += "com.google.code.gson" % "gson" % "2.8.0",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.2" % "test",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.2.0",

    // The "classifier" parts are needed to specify the versions of jacoco that include dependencies and proper manifest
    // files, such that vercors can directly use the jars that are downloaded by sbt as standalone agent/executable jar.
    libraryDependencies += "org.jacoco" % "org.jacoco.cli" % "0.8.7" classifier "nodeps",
    libraryDependencies += "org.jacoco" % "org.jacoco.agent" % "0.8.7" classifier "runtime",

      ThisBuild / scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Ypatmat-exhaust-depth",
      "off"
    ),

    Compile / javacOptions ++= Seq(
      "-Xlint:deprecation",
      "-Xlint:unchecked",
      "-deprecation"
    ),

    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion,
      BuildInfoKey.action("currentBranch") {
        Git.currentBranch
      },
      BuildInfoKey.action("currentShortCommit") {
        Git.currentShortCommit
      },
      BuildInfoKey.action("gitHasChanges") {
        Git.gitHasChanges
      }
    ),
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoPackage := "vct.main",

    /* We want the resources of vercors to be bare files in all cases, so we manually add a resource directory to
    the classpath. That way the resources are not packed into the jar. */
    Compile / unmanagedClasspath += Attributed.blank(sourceDirectory.value / "main" / "universal" / "res" ),
    Test / unmanagedClasspath += Attributed.blank(sourceDirectory.value / "main" / "universal" / "res" ),

    // Disable documentation generation
    Compile / packageDoc / publishArtifact := false,
    Compile / doc / sources := Seq(),

    Universal / mappings ++= Seq(file("README.md") -> "README.md")
      ++ directory("examples")
      // Copy the resources not in the jar and add them to the classpath.
      ++ directory(sourceDirectory.value / "main" / "universal" / "res"),

    scriptClasspath := scriptClasspath.value :+ "../res",

    // Force the main classes, as we have some extra main classes that we don't want to generate run scripts for.
    Compile / discoveredMainClasses := Seq(),
    Compile / mainClass := Some("vct.main.Main"),

    // Add options to run scripts produced by sbt-native-packager. See: https://www.scala-sbt.org/sbt-native-packager/archetypes/java_app/customize.html#via-build-sbt
    Universal / javaOptions ++= Seq (
      // Needed because vercors needs a pretty big stack for some files with deep expressions.
      "-J-Xss128m"
    ),

    // Make publish-local also create a test artifact, i.e., put a jar-file into the local Ivy
    // repository that contains all classes and resources relevant for testing.
    // Other projects, e.g., Carbon or Silicon, can then depend on the Sil test artifact, which
    // allows them to access the Sil test suite.
    Test / packageBin / publishArtifact := true,

    cleanFiles += baseDirectory.value / "bin" / ".classpath",
  )

Global / printMainClasspath := {
    val paths = (vercors / Compile / fullClasspath).value
    val joinedPaths = paths
        .map(_.data)
        .mkString(pathSeparator)
    println(joinedPaths)
}

