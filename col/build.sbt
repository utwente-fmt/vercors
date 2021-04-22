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
  )

  /*
  val compile = FileFunction.cached(streams.value.cacheDirectory / "antlr4", FilesInfo.hash, FilesInfo.hash)(changedSet => {
    println(changedSet)
    if(changedSet.intersect(files.toSet).nonEmpty || true) {
      GenerateHelpers.generate()
      Set(gen / "AbstractRewriter.scala", gen / "RewriteHelpers.scala")
    } else {
      Set()
    }
  })

  compile(files.toSet).toSeq
   */

  ColHelper().generate(files, gen)
}

sourceGenerators in Compile += generateHelpersTask