import NativePackagerHelper._

ThisBuild / turbo := true

enablePlugins(BuildInfoPlugin)
enablePlugins(JavaAppPackaging)
enablePlugins(DebianPlugin)

lazy val viper_api = (project in file("viper"))
lazy val parsers = (project in file("parsers"))

lazy val vercors = (project in file("."))
    .dependsOn(viper_api)
    .dependsOn(parsers)
    .settings(
        name := "Vercors",
        organization := "University of Twente",
        version := "1.2.0",
        maintainer := "VerCors Team <vercors@lists.utwente.nl>",
        packageSummary := "A tool for static verification of parallel programs",
        packageDescription :=
            """VerCors is a tool for static verification of parallel programs. VerCors aims to verify
              |many different concurrency constructs, including: heterogeneous concurrency (Java and C), GPU kernels
              |using barriers and atomics (OpenCL), and compiler directives as used in deterministic parallelism
              |(OpenMP). VerCors is able to prove data-race freedom, memory safety, and functional correctness of
              |(concurrent) programs written in Java, C, OpenCL, OpenMP, and its own Prototypal Verification Language
              |PVL. """.stripMargin.replaceAll("\n", ""),

        libraryDependencies += "commons-io" % "commons-io" % "2.4",
        libraryDependencies += "com.google.code.gson" % "gson" % "2.8.0",
        libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1",
        libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1",
        libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
        libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.4.2" % Test,

        scalaVersion := "2.12.7",

        scalacOptions += "-deprecation",
        scalacOptions += "-feature",
        scalacOptions += "-unchecked",
        scalacOptions += "-Dscalac.patmat.analysisBudget=off",

        javaOptions in (Compile, run) += "-J-Xss128M",
        /* The run script from universal can accept both JVM arguments and application (VerCors) arguments. They are
        separated by "--". We instead want to accept only VerCors arguments, so we force "--" into the arguments. */
        javaOptions in Universal ++= Seq("-J-Xss128M", "--"),

        buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
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
