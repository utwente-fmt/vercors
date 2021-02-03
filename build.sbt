import NativePackagerHelper._
import sys.process._
import java.nio.file.{Files, Path, Paths}
import java.net.URL
import java.util.Comparator
import sbt.internal._

ThisBuild / turbo := true // en wat is daar het praktisch nut van?

enablePlugins(BuildInfoPlugin)
enablePlugins(JavaAppPackaging)
enablePlugins(DebianPlugin)

/* To update viper, replace the hash with the commit hash that you want to point to. It's a good idea to ask people to
 re-import the project into their IDE, as the location of the viper projects below will change. */
val silver_url = uri("git:https://github.com/viperproject/silver.git#v.20.07-release")
val carbon_url = uri("git:https://github.com/viperproject/carbon.git#v.20.07-release")
val silicon_url = uri("git:https://github.com/viperproject/silicon.git#v.20.07-release")

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

lazy val vercors = (project in file("."))
  .dependsOn(hre)
  .dependsOn(col)
  .dependsOn(viper_api)
  .dependsOn(parsers)
  .settings(
    name := "Vercors",
    organization := "University of Twente",
    version := "1.3.0-SNAPSHOT",
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
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.4.2" % Test,

    scalaVersion := "2.12.10",

    scalacOptions in ThisBuild += "-deprecation",
    scalacOptions in ThisBuild += "-feature",
    scalacOptions in ThisBuild += "-unchecked",
    scalacOptions in ThisBuild ++= Seq("-Ypatmat-exhaust-depth", "off"),

    javacOptions in Compile += "-Xlint:deprecation",
    javacOptions in Compile += "-Xlint:unchecked",
    javacOptions in Compile += "-deprecation",
    javacOptions in doc := Seq(),

    javaOptions in (Compile, run) += "-J-Xss128M",
    /* The run script from universal can accept both JVM arguments and application (VerCors) arguments. They are
    separated by "--". We instead want to accept only VerCors arguments, so we force "--" into the arguments. */
    javaOptions in Universal ++= Seq("-J-Xss128M", "--"),

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
    unmanagedClasspath in Compile += Attributed.blank(sourceDirectory.value / "main" / "universal" / "res"),

    // Disable documentation generation
    sources in (Compile, doc) := Seq(),

    mappings in Universal += file("README.md") -> "README.md",
    mappings in Universal ++= directory("examples"),

    // Copy the resources not in the jar and add them to the classpath.
    mappings in Universal ++= directory(sourceDirectory.value / "main" / "universal" / "res"),
    scriptClasspath := scriptClasspath.value :+ "../res",

    // Force the main classes, as we have some extra main classes that we don't want to generate run scripts for.
    discoveredMainClasses in Compile := Seq(),
    mainClass in Compile := Some("vct.main.Main"),

    // Make publish-local also create a test artifact, i.e., put a jar-file into the local Ivy
    // repository that contains all classes and resources relevant for testing.
    // Other projects, e.g., Carbon or Silicon, can then depend on the Sil test artifact, which
    // allows them to access the Sil test suite.
    publishArtifact in(Test, packageBin) := true,
  )
