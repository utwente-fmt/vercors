package vct.col.newrewrite.adt

import vct.col.ast.{AxiomaticDataType, _}
import vct.col.origin.{Origin, PanicBlame}
import vct.col.rewrite.Generation

case object ImportNothing extends ImportADTBuilder("nothing")

case class ImportNothing[Pre <: Generation](importer: ImportADTImporter) extends AImportADT[Pre](importer) {
  private lazy val nothingFile = parse("nothing")

  private lazy val nothingAdt = find[AxiomaticDataType[Post]](nothingFile, "nothing")
  private lazy val nothingAs = find[Function[Post]](nothingFile, "nothing_as")

  override def applyCoercion(e: Expr[Post], coercion: Coercion[Pre])(implicit o: Origin): Expr[Post] = coercion match {
    case CoerceNothingSomething(target) =>
      FunctionInvocation[Post](nothingAs.ref, Seq(e), Seq(dispatch(target)), Nil, Nil)(PanicBlame("coercing from nothing requires nothing."))
    case other => super.applyCoercion(e, other)
  }

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TNothing() => TAxiomatic[Post](nothingAdt.ref, Nil)(t.o)
    case other => rewriteDefault(other)
  }
}
