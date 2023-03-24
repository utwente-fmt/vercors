import java.io.{File, FileOutputStream}
import java.nio.file.{Files, Path}
import scala.collection.parallel.CollectionConverters.seqIsParallelizable
import scala.meta._
import scala.meta.internal.prettyprinters.TreeSyntax
import scala.meta.prettyprinters.Show

object ColHelper {
  def main(args: Array[String]): Unit =
    ColHelper().generate(
      args(0).split(":").map(new File(_)),
      new File(args(1)),
      (f, data) => {
        new File(f.getParent).mkdirs()
        val out = new FileOutputStream(f)
        out.write(data.getBytes)
        out.close()
      }
    )
}

case class ColHelper() {
  
  import ColDefs._
  val info = new ColDescription()

  def generate(input: Seq[File], output: File, writer: (File, String) => Unit): Seq[File] = {
    // Collect the structure of COL into ColDescription
    println(s"[info] [ColHelper] Reading node definitions")
    input.foreach(info.collectInfo)
    info.checkSanity()
    println(s"[info] [ColHelper] Generating helpers for ${info.defs.size} node types")

    // Construct the output path using the col ast package, as well as the name for the package declaration
    val packageOutput: Path = ("java" +: PACKAGE).foldLeft(output.toPath)(_.resolve(_))
    packageOutput.toFile.mkdirs()
    val packageName = PACKAGE.tail.foldLeft[Term.Ref](Term.Name(PACKAGE.head))((t, n) => Term.Select(t, Term.Name(n)))

    val proto = ColProto(info, output, writer)
    proto.make()

    var i = 0

    // Generate the helper files
    Seq[ColHelperMaker](
      ColHelperJavaRewriter(info),
      ColHelperAbstractRewriter(info),
      ColHelperDeserialize(info, proto),
      ColHelperSerialize(info, proto),
      ColHelperRewriteHelpers(info),
      ColHelperRewriteBuilders(info),
      ColHelperSubnodes(info),
      ColHelperComparator(info),
      ColHelperAllScopes(info),
      ColHelperSuccessorsProvider(info),
    ).par.flatMap { maker =>
      println(s"[info] [ColHelper] Generating helpers in ${maker.getClass.getSimpleName}")
      val helpers = maker.make()
      println(s"[info] [ColHelper] Writing out helpers from ${maker.getClass.getSimpleName}")
      helpers.map {
        case (fileName, stats) =>
          val out = packageOutput.resolve(fileName + ".scala").toFile
          val pkg = Pkg(packageName, ColDefs.IMPORTS /*++ List(warner)*/ ++ stats)
          i += 1
          //        val warner = q"class ${Type.Name("Warner" + i.toString)}{ 1 == 'c' }"
          writer(out, pkg.toString())
          out
      }
    }.toIndexedSeq
  }
}