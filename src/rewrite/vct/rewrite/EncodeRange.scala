package vct.rewrite

import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.Generation
import vct.col.rewrite.adt.{ImportADT, ImportADTBuilder, ImportADTImporter}
import vct.col.util.AstBuildHelpers._

object EncodeRange extends ImportADTBuilder("range")

case class EncodeRange[Pre <: Generation](importer: ImportADTImporter)
    extends ImportADT[Pre](importer) {
  lazy val rangeFile = parse("range")

  lazy val rangeSeq = find[Function[Post]](rangeFile, "seq_range")
  lazy val rangeSet = find[Function[Post]](rangeFile, "set_range")

  override def preCoerce(e: Expr[Pre]): Expr[Pre] = {
    implicit val o: Origin = e.o
    e match {
      case SeqMember(x, Range(from, to)) => from <= x && x < to
      case SetMember(x, RangeSet(from, to)) => from <= x && x < to
      case other => other
    }
  }

  override def postCoerce(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o

    e match {
      case Range(from, to) =>
        functionInvocation(
          PanicBlame("seq_range requires nothing"),
          rangeSeq.ref,
          Seq(dispatch(from), dispatch(to)),
        )

      case RangeSet(from, to) =>
        functionInvocation(
          PanicBlame("set_range requires nothing"),
          rangeSet.ref,
          Seq(dispatch(from), dispatch(to)),
        )

      case other => super.postCoerce(other)
    }
  }
}
