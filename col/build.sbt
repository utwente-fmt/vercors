name := "col"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

// Disable documentation generation
Compile / doc / sources := Nil
Compile / packageDoc / publishArtifact := false

lazy val generateHelpersTask = taskKey[Seq[File]]("Generate helpers for the COL AST")

generateHelpersTask := {
  val src = (Compile / sourceDirectory).value / "java" / "vct" / "col" / "ast"
  val gen = (Compile / sourceManaged).value / "java"
  val files = Seq(
    src / "Node.scala",
  )

  val compile = FileFunction.cached(streams.value.cacheDirectory / "removeThisToGenerate-src_managed", FilesInfo.hash)(changedSet => {
    if(changedSet.nonEmpty) {
      ColHelper().generate(files, gen).toSet
    } else {
      Set()
    }
  })

  compile(files.toSet).toSeq
}

Compile / sourceGenerators += generateHelpersTask