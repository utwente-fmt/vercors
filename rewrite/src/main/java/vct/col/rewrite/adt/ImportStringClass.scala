package vct.col.rewrite.adt

import vct.col.ast.RewriteHelpers.RewriteProgram
import vct.col.ast.{AxiomaticDataType, Expr, Intern, PVLStringClassNew, Program, StringClass, StringClassConcat, TStringClass}
import vct.col.rewrite.Generation

case object  ImportStringClass extends ImportADTBuilder("stringClass") {

}

case class ImportStringClass[Pre <: Generation](importer: ImportADTImporter) extends ImportADT[Pre](importer) {

  lazy val stringFile = parse("array")

  lazy val bijectionAdt = find[AxiomaticDataType[Post]](stringFile, "StringBijection")

  var program: Program[Pre] = null;

//  lazy val hasStringClass = program.collectFirst {
//    case cls: StringClass[Pre] => true
//  }.contains(true)
//
//  lazy val usesStringClass = program.collectFirst {
//    case TStringClass() => true
//    case Intern(_) => true
//    case StringClassConcat(_, _) => true
//  }.contains(true)
//
//  lazy val needsStringClass = !hasStringClass && usesStringClass;

  override def postCoerce(program: Program[Pre]): Program[Post] = {
    this.program = program
    super.postCoerce(program)
  }

  override def postCoerce(expr: Expr[Pre]): Expr[Post] = expr match {
    case PVLStringClassNew(str) => // ...?
  }
}
