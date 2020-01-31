lazy val antlrTask = taskKey[Seq[File]]("Generate visitors and listeners from ANTLR grammars")

lazy val parsers = (project in file(".")).settings(
    libraryDependencies += "org.antlr" % "antlr4" % "4.7.2",

    antlrTask := {
        val cp = (dependencyClasspath in Compile).value.files
        val source = (sourceDirectory in Compile).value / "antlr4"
        val target = (sourceManaged in Compile).value / "antlr4" / "vct" / "antlr4" / "generated"
        val log = streams.value.log

        val cachedCompile = FileFunction.cached(streams.value.cacheDirectory / "antlr4", FilesInfo.lastModified, FilesInfo.exists) {
            grammarFiles: Set[File] => {
                for(grammarFile <- grammarFiles) {
                    val exitCode = scala.sys.process.Process("java", Seq(
                        "-cp", "/home/pieter/antlr4/classes/artifacts/antlr4_jar/antlr4.jar",
                        "org.antlr.v4.Tool",
                        "-o", target.toString,
                        "-lib", ((unmanagedBase in Compile).value / "antlr4").toString,
                        "-listener", "-visitor",
                        "-scala-extractor-objects",
                        "-package", "vct.antlr4.generated",
                        grammarFile.toString
                    )) ! log

                    if(exitCode != 0) {
                        sys.error(s"Antlr4 failed with exit code $exitCode")
                    }
                }
                (target ** "*.java").get.toSet ++ (target ** "*.scala").get.toSet
            }
        }

        cachedCompile((source ** "*.g4").get.toSet).toSeq
    },

    sourceGenerators in Compile += (antlrTask in Compile).taskValue,
    managedSourceDirectories in Compile += (sourceManaged in Compile).value / "antlr4",
)
