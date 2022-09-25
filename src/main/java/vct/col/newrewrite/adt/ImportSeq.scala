package vct.col.newrewrite.adt

import vct.col.ast._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewritten}

case object ImportSeq extends ImportADTBuilder("seq")

case class ImportSeq[Pre <: Generation](importer: ImportADTImporter) extends ImportADT[Pre](importer) {
  private lazy val seqFile = parse("seq")

  private lazy val seqAdt = find[AxiomaticDataType[Post]](seqFile, "seq")
  private lazy val seqLength = find[ADTFunction[Post]](seqAdt, "seq_length")
  private lazy val seqIndex = find[ADTFunction[Post]](seqAdt, "seq_index")
  private lazy val seqContains = find[ADTFunction[Post]](seqAdt, "seq_contains")
  private lazy val seqIndexSkolem = find[ADTFunction[Post]](seqAdt, "seq_idx_skolem")
  private lazy val seqEqual = find[ADTFunction[Post]](seqAdt, "seq_equal")
  private lazy val seqDiffSkolem = find[ADTFunction[Post]](seqAdt, "seq_diff_skolem")
  private lazy val seqEmpty = find[ADTFunction[Post]](seqAdt, "seq_empty")
  private lazy val seqSingleton = find[ADTFunction[Post]](seqAdt, "seq_singleton")
  private lazy val seqConcat = find[ADTFunction[Post]](seqAdt, "seq_concat")
  private lazy val seqRange = find[ADTFunction[Post]](seqAdt, "seq_range")
  private lazy val seqUpdate = find[ADTFunction[Post]](seqAdt, "seq_update")
  private lazy val seqTake = find[ADTFunction[Post]](seqAdt, "seq_take")
  private lazy val seqDrop = find[ADTFunction[Post]](seqAdt, "seq_drop")
  private lazy val seqArithAdd = find[ADTFunction[Post]](seqAdt, "seq_+")
  private lazy val seqArithSub = find[ADTFunction[Post]](seqAdt, "seq_-")

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TSeq(t) => TAxiomatic(seqAdt.ref, Seq(dispatch(t)))
    case other => rewriteDefault(other)
  }

  def typeArgs(xs: Expr[Pre]): Option[(Ref[Post, AxiomaticDataType[Post]], Seq[Type[Post]])] =
    typeArgs(xs.t.asSeq.get.element)

  def typeArgs(t: Type[Pre]): Option[(Ref[Post, AxiomaticDataType[Post]], Seq[Type[Post]])] =
    Some((seqAdt.ref, Seq(dispatch(t))))

  override def postCoerce(e: Expr[Pre]): Expr[Post] = e match {
    case Size(xs) if xs.t.asSeq.nonEmpty =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(xs),
        ref = seqLength.ref,
        args = Seq(dispatch(xs)),
      )(e.o)

    case SeqSubscript(xs, i) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(xs),
        ref = seqIndex.ref,
        args = Seq(dispatch(xs), dispatch(i)),
      )(e.o)

    case SeqMember(x, xs) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(xs),
        ref = seqContains.ref,
        args = Seq(dispatch(xs), dispatch(x)),
      )(e.o)

    case eq @ Eq(xs, ys) if eq.comparisonType.asSeq.nonEmpty =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(eq.comparisonType.asSeq.get.element),
        ref = seqEqual.ref,
        args = Seq(dispatch(xs), dispatch(ys)),
      )(e.o)

    case eq @ Neq(xs, ys) if eq.comparisonType.asSeq.nonEmpty =>
      Not(ADTFunctionInvocation[Post](
        typeArgs = typeArgs(eq.comparisonType.asSeq.get.element),
        ref = seqEqual.ref,
        args = Seq(dispatch(xs), dispatch(ys)),
      )(e.o))(e.o)

    case concat @ Concat(xs, ys) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(concat.t.asSeq.get.element),
        ref = seqConcat.ref,
        args = Seq(dispatch(xs), dispatch(ys)),
      )(e.o)

    case LiteralSeq(t, Nil) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(t),
        ref = seqEmpty.ref,
        args = Nil,
      )(e.o)

    case LiteralSeq(t, x :: xs) =>
      xs.foldLeft(
        ADTFunctionInvocation[Post](
          typeArgs = typeArgs(t),
          ref = seqSingleton.ref,
          args = Seq(dispatch(x)),
        )(e.o)
      ) {
        case (init, next) =>
          ADTFunctionInvocation[Post](
            typeArgs = typeArgs(t),
            ref = seqConcat.ref,
            args = Seq(init, ADTFunctionInvocation[Post](
              typeArgs = typeArgs(t),
              ref = seqSingleton.ref,
              args = Seq(dispatch(next))
            )(e.o))
          )(e.o)
      }

    case Range(from, to) =>
      ADTFunctionInvocation[Post](
        typeArgs = Some((seqAdt.ref, Seq(TInt()))),
        ref = seqRange.ref,
        args = Seq(dispatch(from), dispatch(to)),
      )(e.o)

    case SeqUpdate(xs, i, x) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(xs),
        ref = seqUpdate.ref,
        args = Seq(dispatch(xs), dispatch(i), dispatch(x)),
      )(e.o)

    case Take(xs, n) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(xs),
        ref = seqTake.ref,
        args = Seq(dispatch(xs), dispatch(n)),
      )(e.o)

    case Drop(xs, n) =>
      ADTFunctionInvocation[Post](
        typeArgs = typeArgs(xs),
        ref = seqDrop.ref,
        args = Seq(dispatch(xs), dispatch(n)),
      )(e.o)

    case other => rewriteDefault(other)
  }
}
