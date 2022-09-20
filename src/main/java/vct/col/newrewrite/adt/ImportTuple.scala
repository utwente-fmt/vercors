package vct.col.newrewrite.adt

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.Generation

case object ImportTuple extends ImportADTBuilder("tuple")

case class ImportTuple[Pre <: Generation](importer: ImportADTImporter) extends AImportADT[Pre](importer) {
  private lazy val tupleFile = parse("tuple")

  private lazy val tupleAdt = find[AxiomaticDataType[Post]](tupleFile, "tuple")
  private lazy val tupleTup = find[ADTFunction[Post]](tupleAdt, "tup")
  private lazy val tupleFst = find[ADTFunction[Post]](tupleAdt, "fst")
  private lazy val tupleSnd = find[ADTFunction[Post]](tupleAdt, "snd")

  def tupFst(e: Expr[Post], fstType: Type[Post], sndType: Type[Post])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation[Post](
      Some((tupleAdt.ref, Seq(fstType, sndType))),
      tupleFst.ref, Seq(e),
    )

  def tupSnd(e: Expr[Post], fstType: Type[Post], sndType: Type[Post])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation[Post](
      Some((tupleAdt.ref, Seq(fstType, sndType))),
      tupleSnd.ref, Seq(e),
    )

  def tup(fst: Expr[Post], fstType: Type[Post], snd: Expr[Post], sndType: Type[Post])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation[Post](
      Some((tupleAdt.ref, Seq(fstType, sndType))),
      tupleTup.ref, Seq(fst, snd),
    )

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TTuple(Seq(t1, t2)) => TAxiomatic(tupleAdt.ref, Seq(dispatch(t1), dispatch(t2)))
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case LiteralTuple(Seq(t1, t2), Seq(v1, v2)) =>
      tup(dispatch(v1), dispatch(t1), dispatch(v2), dispatch(t2))(e.o)
    case TupGet(tup, 0) =>
      val ts = tup.t.asTuple.get.elements.map(dispatch)
      tupFst(dispatch(tup), ts(0), ts(1))(e.o)
    case TupGet(tup, 1) =>
      val ts = tup.t.asTuple.get.elements.map(dispatch)
      tupSnd(dispatch(tup), ts(0), ts(1))(e.o)
    case other => rewriteDefault(other)
  }
}
