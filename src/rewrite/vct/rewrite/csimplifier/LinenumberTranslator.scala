package vct.rewrite.csimplifier

import com.typesafe.scalalogging.LazyLogging
import hre.io.RWFile
import vct.col.ast._
import vct.col.print.{Ctx, Namer}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilderArg}

import java.io.Writer
import java.nio.file.Path

case object LinenumberTranslator extends RewriterBuilderArg[Path] {
  override def key: String = "linenumberTranslator"
  override def desc: String =
    "export a JSON file that maps the line numbers after cSimplifier translations to the original line number"
}

/** "Rewriter" that actually leaves the AST unchanged, but exports a JSON file
  * which maps line numbers in the current AST to their original position
  *
  * @param path
  *   path to generated C file. Exported JSON will be in "<path>.lines.json"
  */
case class LinenumberTranslator[Pre <: Generation](path: Path)
    extends Rewriter[Pre] with LazyLogging {

  /** dummy rewriter for entire program, actually just exporting the line number
    * JSON
    */
  override def dispatch(program: Program[Pre]): Program[Post] = {
    exportLinenumberMapping(program)
    program.rewriteDefault()
  }

  private def exportLinenumberMapping(program: Program[Pre]): Unit = {
    // nodes whose line number we're interested in, such as loops and method calls
    val relevant = program.collect {
      case l: Loop[Pre] => l
      case p: Procedure[Pre] => p
      case i: InvokeProcedure[Pre] => i
      case i: ProcedureInvocation[Pre] => i
    }
    val namer = Namer[Generation](Ctx.C, useSourceNames = true)
    val names = namer.finish
    implicit val ctx: Ctx = Ctx(
      syntax = Ctx.C,
      names = names.asInstanceOf[Map[Declaration[_], String]],
    )

    // pairs "(<new linenumber>, <origin (string)>)"
    val mapping = program.layout.getLinenumberMapping(relevant)
    // generated methods have no proper origin -> filter them out
    val filtered = mapping.filter(p => p._2 != "multiple files")
    // name for JSON file
    val filename = path.resolveSibling(path.getFileName + ".lines.json")
    // write JSON
    RWFile(filename).write(w => print_mapping(w, filtered))
  }

  /** turn the given line number mapping into a JSON string
    * @param mapping
    *   map of line number in output file to its origin (string)
    */
  private def print_mapping(
      writer: Writer,
      mapping: Iterable[(Int, String)],
  ): Unit = {
    writer.append("{\n  ")
    writer.append(
      mapping.map(p =>
        "\"" + path.getFileName + ":" + p._1.toString + "\" : \"" + p._2 + "\""
      ).mkString(",\n  ")
    )
    writer.append("\n}")
  }

}
