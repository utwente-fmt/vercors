name := "col"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1"
sources in (Compile, doc) := Seq()

lazy val generateHelpersTask = taskKey[Seq[File]]("Generate helpers for the COL AST")

generateHelpersTask := {
  val src = (sourceDirectory in Compile).value / "java" / "vct" / "col" / "ast"
  val gen = (sourceManaged in Compile).value / "java"
  val files = Seq(
    src / "Ast.scala",

    src / "Expr.scala",
    src / "Statement.scala",
    src / "Type.scala",
    src / "Declaration.scala",

    src / "Silver.scala",
    src / "C.scala",
    src / "Java.scala",
  )

  val compile = FileFunction.cached(streams.value.cacheDirectory / "removeThisToGenerate-src_managed", FilesInfo.hash)(changedSet => {
    println(changedSet)
    if(changedSet.nonEmpty) {
      ColHelper().generate(files, gen).toSet
    } else {
      Set()
    }
  })

  compile(files.toSet).toSeq
}

sourceGenerators in Compile += generateHelpersTask