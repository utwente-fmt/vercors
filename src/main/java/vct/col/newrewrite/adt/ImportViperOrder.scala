package vct.col.newrewrite.adt

import vct.col.ast.Program
import vct.col.ast.RewriteHelpers.RewriteProgram
import vct.col.rewrite.Generation

case object ImportViperOrder extends ImportADTBuilder("viper_order")

case class ImportViperOrder[Pre <: Generation](importer: ImportADTImporter) extends ImportADT[Pre](importer) {
  override def dispatch(program: Program[Pre]): Program[Post] = {
    program.rewrite(declarations = globalDeclarations.collect {
      parse("viper_order")
      program.declarations.foreach(dispatch)
    }._1)
  }
}
