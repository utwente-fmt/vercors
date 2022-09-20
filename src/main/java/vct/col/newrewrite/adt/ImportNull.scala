package vct.col.newrewrite.adt

import vct.col.ast._
import vct.col.rewrite.Generation

case class ImportNull[Pre <: Generation](importer: ImportADTImporter) extends AImportADT[Pre](importer) {
  private lazy val nullFile = parse("null")

  private lazy val nullAdt = find[AxiomaticDataType[Post]](nullFile, "t_null")
  private lazy val nullValue = find[ADTFunction[Post]](nullAdt, "v_null")

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TNull() => TAxiomatic[Post](nullAdt.ref, Nil)(t.o)
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case Null() =>
      // Uncoerced, so will become TNull
      ADTFunctionInvocation[Post](None, nullValue.ref, Nil)(e.o)
    case other => rewriteDefault(other)
  }
}

