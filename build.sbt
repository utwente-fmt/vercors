import NativePackagerHelper._
import java.io.File.pathSeparator
import sbt.internal._

ThisBuild / turbo := true // en wat is daar het praktisch nut van?
ThisBuild / scalaVersion := "2.13.5"
ThisBuild / fork := true

ThisBuild / pushRemoteCacheTo := Some(MavenCache("local-cache", file("tmp/vercors-build-cache")))

enablePlugins(BuildInfoPlugin)
enablePlugins(JavaAppPackaging)
enablePlugins(DebianPlugin)

/* To update viper, replace the hash with the commit hash that you want to point to. It's a good idea to ask people to
 re-import the project into their IDE, as the location of the viper projects below will change. */
val silver_url = uri("git:https://github.com/viperproject/silver.git#11bde93e486e983141c01ac7df270e9f06e8ab06")
val carbon_url = uri("git:https://github.com/viperproject/carbon.git#44f9225dcde2374c3b8051b6d56ac88c7c4ffdd5")
val silicon_url = uri("git:https://github.com/viperproject/silicon.git#f844927fe6f54c3dbc5adbccfa011034c8036640")

/*
buildDepdendencies.classpath contains the mapping from project to a list of its dependencies. The viper projects silver,
silicon and carbon specify their dependencies as a regular sbt subproject: they expect a symlink in the project root to
the relevant project. Instead, we replace those dependencies by a reference to the repository as above. So e.g.
"the silver project at hg:carbon" becomes "the silver project at hg:silver". All other dependencies are left alone.
 */
Global / buildDependencies := {
  val log = sLog.value
  val oldDeps = (Global / buildDependencies).value
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
lazy val rewrite = (project in file("rewrite")).dependsOn(hre, col)
lazy val parsers = (project in file("parsers")).dependsOn(hre, col)
lazy val viper = (project in file("viper")).dependsOn(hre, col, parsers, silver_ref, carbon_ref, silicon_ref)

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

carbon_ref / unmanagedResources / excludeFilter := "logback*.xml"
silicon_ref / unmanagedResources / excludeFilter := "logback*.xml"

// Disable doc generation in all viper projects
carbon_ref / packageDoc / publishArtifact := false
silver_ref / packageDoc / publishArtifact := false
silicon_ref / packageDoc / publishArtifact := false
ProjectRef(silver_url, "common") / packageDoc / publishArtifact := false
ProjectRef(carbon_url, "common") / packageDoc / publishArtifact := false
ProjectRef(silicon_url, "common") / packageDoc / publishArtifact := false
lazy val printMainClasspath = taskKey[Unit]("Prints classpath of main vercors executable")
lazy val printTestClasspath = taskKey[Unit]("Prints classpath of test vercors executable")
lazy val printRuntimeClasspath = taskKey[Unit]("Prints classpath of vercors in runtime")
lazy val benchPrintExternalDeps = taskKey[Unit]("For util/bench/flatten-sources.sh: print only the external dependencies, available without compilation.")
lazy val benchPrintSources = taskKey[Unit]("For util/bench/flatten-sources.sh: print all source directories, including viper.")

lazy val fetchGitInfo = taskKey[Seq[String]]("Explicitly depend on git information to generate BuildInfo")

fetchGitInfo := {
  Seq(Git.currentBranch, Git.currentCommit, Git.currentShortCommit, Git.gitHasChanges.toString)
}

Compile / buildInfo := (Compile / buildInfo).dependsOn(fetchGitInfo).value

lazy val vercors: Project = (project in file("."))
  .dependsOn(hre, col, rewrite, viper, parsers)
  .aggregate(hre, col, rewrite, viper, parsers)
  .settings(
    fork := true,
    name := "Vercors",
    organization := "nl.utwente",
    version := "2.0.0-beta.1",
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
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.7",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % "test",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",

    // The "classifier" parts are needed to specify the versions of jacoco that include dependencies and proper manifest
    // files, such that vercors can directly use the jars that are downloaded by sbt as standalone agent/executable jar.
    libraryDependencies += "org.jacoco" % "org.jacoco.cli" % "0.8.7" classifier "nodeps",
    libraryDependencies += "org.jacoco" % "org.jacoco.agent" % "0.8.7" classifier "runtime",

    ThisBuild / scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
//      "-Xno-patmat-analysis",
//      "-Ystatistics:typer",
//      "-Xprint:typer",
//      "-Ycache-plugin-class-loader:last-modified",
//      "-Xplugin:/home/pieter/.cache/coursier/v1/https/repo1.maven.org/maven2/io/leonard/scalac-profiling_2.13/0.0.1/scalac-profiling_2.13-0.0.1.jar",
//      "-P:scalac-profiling:no-profiledb",
//      "-P:scalac-profiling:show-profiles",
//      "-P:scalac-profiling:sourceroot:/home/pieter/vercors/",
//      "-Ypatmat-debug",
    ),

    Compile / javacOptions ++= Seq(
      "-Xlint:deprecation",
      "-Xlint:unchecked",
      "-deprecation"
    ),

    Runtime / javacOptions ++= Seq(
      "-Xlint:deprecation",
      "-Xlint:unchecked",
      "-deprecation"
    ),

    Test / javacOptions ++= Seq(
      "-Xlint:deprecation",
      "-Xlint:unchecked",
      "-deprecation"
    ),

    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion,
      BuildInfoKey.action("currentBranch") {
        Git.currentBranch
      },
      BuildInfoKey.action("currentCommit") {
        Git.currentCommit
      },
      BuildInfoKey.action("currentShortCommit") {
        Git.currentShortCommit
      },
      BuildInfoKey.action("gitHasChanges") {
        Git.gitHasChanges
      },
      "silverCommit" -> BuildUtil.commitFromGitUrl(silver_url.toString),
      "siliconCommit" -> BuildUtil.commitFromGitUrl(silicon_url.toString),
      "carbonCommit" -> BuildUtil.commitFromGitUrl(carbon_url.toString)
    ),
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoPackage := "vct.main",

    /* We want the resources of vercors to be bare files in all cases, so we manually add a resource directory to
    the classpath. That way the resources are not packed into the jar. */
    Compile / unmanagedClasspath += Attributed.blank(sourceDirectory.value / "main" / "universal" / "res"),
    Compile / unmanagedClasspath += Attributed.blank(sourceDirectory.value / "main" / "universal" / "deps"),
    Runtime / unmanagedClasspath += Attributed.blank(sourceDirectory.value / "main" / "universal" / "res"),
    Runtime / unmanagedClasspath += Attributed.blank(sourceDirectory.value / "main" / "universal" / "deps"),
    Test / unmanagedClasspath += Attributed.blank(sourceDirectory.value / "main" / "universal" / "res"),
    Test / unmanagedClasspath += Attributed.blank(sourceDirectory.value / "main" / "universal" / "deps"),

    // Disable documentation generation
    Compile / packageDoc / publishArtifact := false,
    Compile / doc / sources := Seq(),

    Test / parallelExecution := false,

    Universal / mappings ++= Seq(file("README.md") -> "README.md")
      ++ directory("examples")
      // Copy the resources not in the jar and add them to the classpath.
      ++ directory(sourceDirectory.value / "main" / "universal" / "res"),

    Universal / packageBin / mappings ++= directory(sourceDirectory.value / "main" / "universal" / "deps" / "win") map { case (f, path) => f -> s"res/$path" },
    Universal / packageZipTarball / mappings ++= directory(sourceDirectory.value / "main" / "universal" / "deps" / "darwin") map { case (f, path) => f -> s"res/$path" },
    Debian /  linuxPackageMappings ++= directory(sourceDirectory.value / "main" / "universal" / "deps" / "unix") map { case (f, path) => packageMapping(f -> s"usr/share/${normalizedName.value}/res/$path") },

    // Sets the classpath as described on the below page
    // https://sbt-native-packager.readthedocs.io/en/latest/recipes/longclasspath.html
    // To circumvent the long classpath problem
    // At the time of writing (2021-10-08) the other two workarounds described
    // on that page seem to be broken.
    // Both result in "class vct.main.Main" not found when running vercors.
    // See: https://github.com/sbt/sbt-native-packager/issues/1466
    scriptClasspath := Seq("*", "../res"),

    // Force the main classes, as we have some extra main classes that we don't want to generate run scripts for.
    Compile / discoveredMainClasses := Seq(),
    Compile / mainClass := Some("vct.main.Main"),

    // Add options to run scripts produced by sbt-native-packager. See: https://www.scala-sbt.org/sbt-native-packager/archetypes/java_app/customize.html#via-build-sbt
    Universal / javaOptions ++= Seq (
      // Needed because vercors needs a pretty big stack for some files with deep expressions.
      "-J-Xss128M"
    ),

    Runtime / javaOptions ++= Seq (
      // Needed because vercors needs a pretty big stack for some files with deep expressions.
      "-Xss256M"
    ),

    Test / javaOptions ++= Seq (
      // Needed because vercors needs a pretty big stack for some files with deep expressions.
      "-Xss256M"
    ),

    // Make publish-local also create a test artifact, i.e., put a jar-file into the local Ivy
    // repository that contains all classes and resources relevant for testing.
    // Other projects, e.g., Carbon or Silicon, can then depend on the Sil test artifact, which
    // allows them to access the Sil test suite.
    Test / packageBin / publishArtifact := true,

    cleanFiles += baseDirectory.value / "bin" / ".classpath",
  )

Global / printMainClasspath := {
  val paths = (Compile / fullClasspath).value
  val joinedPaths = paths
    .map(_.data)
    .mkString(pathSeparator)
  println(joinedPaths)
}

Global / printTestClasspath := {
  val paths = (Test / fullClasspath).value
  val joinedPaths = paths
    .map(_.data)
    .mkString(pathSeparator)
  println(joinedPaths)
}

Global / printRuntimeClasspath := {
  val paths = (Runtime / fullClasspath).value
  val joinedPaths = paths
    .map(_.data)
    .mkString(pathSeparator)
  println(joinedPaths)
}

Global / benchPrintExternalDeps := {
  println(
    (Runtime / externalDependencyClasspath).value
      .map(_.data.toString.replace("\"", "\\\""))
      .map(entry => "Attributed.blank(file(\"" + entry + "\"))")
      .mkString("Seq(", ", ", ")")
  )
}

Global / benchPrintSources := {
  println((vercors / baseDirectory).value / "src")
  println((vercors / baseDirectory).value / "target" / "scala-2.13" / "src_managed")
  println((hre / baseDirectory).value / "src")
  println((col / baseDirectory).value / "src")
  println((col / baseDirectory).value / "target" / "scala-2.13" / "src_managed")
  println((parsers / baseDirectory).value / "src")
  println((parsers / baseDirectory).value / "target" / "scala-2.13" / "src_managed")
  println((rewrite / baseDirectory).value / "src")
  println((viper / baseDirectory).value / "src")
  println((silver_ref / baseDirectory).value / "src")
  println((carbon_ref / baseDirectory).value / "src")
  println((silicon_ref / baseDirectory).value / "common" / "src")
  println((silicon_ref / baseDirectory).value / "src")
}
