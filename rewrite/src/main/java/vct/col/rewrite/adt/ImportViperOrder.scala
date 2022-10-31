package vct.col.rewrite.adt

import vct.col.ast.{ApplicableContract, Program}
import vct.col.ast.RewriteHelpers.RewriteProgram
import vct.col.rewrite.Generation

case object ImportViperOrder extends ImportADTBuilder("viper_order")

case class ImportViperOrder[Pre <: Generation](importer: ImportADTImporter) extends ImportADT[Pre](importer) {
  override def dispatch(program: Program[Pre]): Program[Post] = {
    program.rewrite(declarations = globalDeclarations.collect {
      if(program.transSubnodes.collectFirst { case contract: ApplicableContract[Pre] if contract.decreases.nonEmpty => () }.nonEmpty) {
        parse("viper_order")
      }
      program.declarations.foreach(dispatch)
    }._1)
  }
}
