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
  val gen = (Compile / sourceManaged).value
  val files = Seq(
    src / "Node.scala",
  )

  val compile = FileFunction.cached(streams.value.cacheDirectory / "removeThisToGenerate-src_managed", FilesInfo.hash)(changedSet => {
    if(changedSet.nonEmpty) {
      ColHelper().generate(files, gen, (f: File, c: String) => IO.write(f, c)).toSet
    } else {
      Set()
    }
  })

  compile(files.toSet).toSeq
}

Compile / sourceGenerators += generateHelpersTask

Compile / PB.targets := Seq(
  scalapb.gen(flatPackage = true) -> (Compile / sourceManaged).value / "scalapb"
)

Compile / PB.protoSources ++= Seq(
  (Compile / sourceManaged).value / "protobuf"
)

// The generation of protobuf helpers has to depend on the generation of col.proto
Compile / PB.generate := (Compile / PB.generate).dependsOn(generateHelpersTask).value