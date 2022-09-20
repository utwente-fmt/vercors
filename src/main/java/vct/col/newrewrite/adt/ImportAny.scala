package vct.col.newrewrite.adt

import vct.col.ast.{AxiomaticDataType, CoerceSomethingAny, Coercion, Expr, Function, FunctionInvocation, TAny, TAxiomatic, Type}
import vct.col.origin.{Origin, PanicBlame}
import vct.col.rewrite.Generation

case object ImportAny extends ImportADTBuilder("any")

case class ImportAny[Pre <: Generation](importer: ImportADTImporter) extends AImportADT[Pre](importer) {
  private lazy val anyFile = parse("any")

  private lazy val anyAdt = find[AxiomaticDataType[Post]](anyFile, "any")
  private lazy val anyFrom = find[Function[Post]](anyFile, "as_any")

  override def applyCoercion(e: Expr[Post], coercion: Coercion[Pre])(implicit o: Origin): Expr[Post] = coercion match {
    case CoerceSomethingAny(source) =>
      FunctionInvocation[Post](anyFrom.ref, Seq(e), Seq(dispatch(source)), Nil, Nil)(PanicBlame("coercing to any requires nothing."))
    case other => super.applyCoercion(e, other)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TAny() => TAxiomatic(anyAdt.ref, Nil)
    case other => rewriteDefault(other)
  }
}
