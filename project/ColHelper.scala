import java.io.File
import java.nio.file.{Files, Path}
import scala.meta._

case class ColHelper() {
  import ColDefs._
  val info = new ColDescription()

  def generate(input: Seq[File], output: File): Seq[File] = {
    // Collect the structure of COL into ColDescription
    input.foreach(info.collectInfo)
    println(s"Generating helpers for ${info.defs.size} node types")

    // Construct the output path using the col ast package, as well as the name for the package declaration
    val packageOutput: Path = PACKAGE.foldLeft(output.toPath)(_.resolve(_))
    packageOutput.toFile.mkdirs()
    val packageName = PACKAGE.tail.foldLeft[Term.Ref](Term.Name(PACKAGE.head))((t, n) => Term.Select(t, Term.Name(n)))

    // Generate the helper files
    Seq[(String, () => List[Stat])](
      ("AbstractRewriter", ColHelperAbstractRewriter(info).make),
      ("RewriteHelpers", ColHelperRewriteHelpers(info).make),
      // Might increase compilation time by a bit:
//      ("RewriteBuilders", ColHelperRewriteBuilders(info).make),
//      ("JavaRewriter", ColHelperJavaRewriter(info).make),
      ("Subnodes", ColHelperSubnodes(info).make),
      ("Comparator", ColHelperComparator(info).make),
      ("AllScopes", ColHelperAllScopes(info).make),
      ("SuccessorsProvider", ColHelperSuccessorsProvider(info).make),
    ).map {
      case (fileName, maker) =>
        val out = packageOutput.resolve(fileName + ".scala")
        Files.deleteIfExists(out)
        Files.writeString(out, Pkg(packageName, ColDefs.IMPORTS ++ maker()).toString())
        out.toFile
    }
  }
}