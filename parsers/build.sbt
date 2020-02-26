lazy val antlrTask = taskKey[Seq[File]]("Generate visitors and listeners from ANTLR grammars")

lazy val parsers = (project in file(".")).settings(
    libraryDependencies += "antlr" % "antlr" % "4.8-extractors-1" from
      "https://github.com/niomaster/antlr4/releases/download/4.8-extractors-1/antlr4.jar",

    antlrTask := {
        val cp = (dependencyClasspath in Compile).value.files
        val src = (sourceDirectory in Compile).value / "antlr4"
        val lib = (unmanagedBase in Compile).value / "antlr4"
        val target = (sourceManaged in Compile).value / "antlr4" / "vct" / "antlr4" / "generated"
        val log = streams.value.log

        val compileSets: Seq[(java.io.File, Boolean, Set[java.io.File])] = Seq(
            (lib / "LangCLexer.g4", false,
              Set(lib / "SpecLexer.g4")),
            (lib / "LangJavaLexer.g4", false,
              Set(lib / "SpecLexer.g4")),
            (src / "PVL.g4", true,
              Set(lib / "SpecParser.g4", lib / "SpecLexer.g4")),
            (src / "CParser.g4", true,
              Set(lib / "SpecParser.g4", lib / "SpecLexer.g4",
                  lib / "LangCParser.g4", lib / "LangCLexer.g4")),
            (src / "JavaParser.g4", true,
              Set(lib / "SpecParser.g4", lib / "SpecLexer.g4",
                  lib / "LangJavaParser.g4", lib / "LangJavaLexer.g4")),
            (src / "omp.g4", true, Set()),
        )

        val allInputFiles: Set[java.io.File] =
            compileSets.foldLeft(Set[java.io.File]()) {
                case (set, (target, _, deps)) => set + target ++ deps
            }

        val cachedCompile = FileFunction.cached(streams.value.cacheDirectory / "antlr4", FilesInfo.hash, FilesInfo.hash) {
            changedSet: Set[File] => {
                for((genTarget, isParser, deps) <- compileSets) {
                    val extraArgs = if (isParser) {
                        Seq("-listener", "-visitor", "-scala-extractor-objects")
                    } else {
                        Seq()
                    }

                    if (changedSet.contains(genTarget) || !changedSet.intersect(deps).isEmpty) {
                        val exitCode = scala.sys.process.Process("java", Seq(
                            "-cp", Path.makeString(cp),
                            "org.antlr.v4.Tool",
                            "-o", target.toString,
                            "-lib", lib.toString,
                            "-package", "vct.antlr4.generated",
                            genTarget.toString
                        ) ++ extraArgs) ! log

                        if(exitCode != 0) {
                            sys.error(s"Antlr4 failed with exit code $exitCode")
                        }
                    }
                }

                // Grab all the generated files as our output
                (target ** "*.java").get.toSet ++ (target ** "*.scala").get.toSet
            }
        }

        cachedCompile(allInputFiles).toSeq
    },

    sourceGenerators in Compile += (antlrTask in Compile).taskValue,
    managedSourceDirectories in Compile += (sourceManaged in Compile).value / "antlr4",
)
