package vct.col.rewrite.adt

import vct.col.ast.RewriteHelpers.RewriteProgram
import vct.col.ast.{AxiomaticDataType, Declaration, Expr, Intern, PVLStringClassNew, Procedure, ProcedureInvocation, Program, StringClass, StringClassConcat, TStringClass}
import vct.col.origin.PanicBlame
import vct.col.rewrite.Generation
import vct.col.serialize.Expr.V.Empty.procedureInvocation

case object  ImportStringClass extends ImportADTBuilder("stringClass") {

}

case class ImportStringClass[Pre <: Generation](importer: ImportADTImporter) extends ImportADT[Pre](importer) {

  lazy val stringFile = parse("stringClass")
  lazy val constructor: Procedure[Post] = find[Procedure[Post]](stringFile, "constructor")

  var program: Program[Pre] = null;

  lazy val hasStringClass = program.collectFirst {
    case cls: StringClass[Pre] => true
  }.contains(true)

  lazy val usesStringClass = program.collectFirst {
    case TStringClass() => true
    case Intern(_) => true
    case StringClassConcat(_, _) => true
  }.contains(true)

  lazy val needsStringClass = !hasStringClass && usesStringClass;

  override def postCoerce(program: Program[Pre]): Program[Post] = {
    this.program = program

    val p = globalBlame.having(program.blame) {
      program.rewrite(declarations = globalDeclarations.collect {
        if (needsStringClass) {
          constructor
        }

        program.declarations.foreach(dispatch)
      }._1)
    }
    p
  }

  override def postCoerce(expr: Expr[Pre]): Expr[Post] = expr match {
    case PVLStringClassNew(str) => ProcedureInvocation[Post](
        constructor.ref,
        Seq(dispatch(str)),
        Nil,
        Nil,
        Nil,
        Nil
      )(PanicBlame("String class instance can always be constructed"))(expr.o)
    case _ => rewriteDefault(expr)
  }
}
