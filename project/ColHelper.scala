import java.io.File
import java.nio.file.{Files, Path}
import scala.meta._

case class ColHelper() {
  import ColDefs._
  val info = new ColDescription()

  def generate(input: Seq[File], output: File, writer: (File, String) => Unit): Seq[File] = {
    // Collect the structure of COL into ColDescription
    input.foreach(info.collectInfo)
    println(s"Generating helpers for ${info.defs.size} node types")

    // Construct the output path using the col ast package, as well as the name for the package declaration
    val packageOutput: Path = PACKAGE.foldLeft(output.toPath)(_.resolve(_))
    packageOutput.toFile.mkdirs()
    val packageName = PACKAGE.tail.foldLeft[Term.Ref](Term.Name(PACKAGE.head))((t, n) => Term.Select(t, Term.Name(n)))

    var i = 0

    // Generate the helper files
    Seq[List[(String, List[Stat])]](
      ColHelperAbstractRewriter(info).make(),
      ColHelperRewriteHelpers(info).make(),
      ColHelperRewriteBuilders(info).make(),
      ColHelperJavaRewriter(info).make(),
      ColHelperSubnodes(info).make(),
      ColHelperComparator(info).make(),
      ColHelperAllScopes(info).make(),
      ColHelperSuccessorsProvider(info).make(),
    ).flatten.map {
      case (fileName, stats) =>
        val out = packageOutput.resolve(fileName + ".scala").toFile
        i += 1
//        val warner = q"class ${Type.Name("Warner" + i.toString)}{ 1 == 'c' }"
        writer(out, Pkg(packageName, ColDefs.IMPORTS /*++ List(warner)*/ ++ stats).toString())
        out
    }
  }
}