package vct.col.newrewrite.adt

import vct.col.ast._
import vct.col.rewrite.Generation

case class ImportVoid[Pre <: Generation](importer: ImportADTImporter) extends AImportADT[Pre](importer) {
  private lazy val voidFile = parse("void")

  private lazy val voidAdt = find[AxiomaticDataType[Post]](voidFile, "void")
  private lazy val voidUnit = find[ADTFunction[Post]](voidAdt, "unit")

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TVoid() => TAxiomatic(voidAdt.ref, Nil)
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case Void() =>
      ADTFunctionInvocation[Post](None, voidUnit.ref, Nil)(e.o)
    case other => rewriteDefault(other)
  }
}

