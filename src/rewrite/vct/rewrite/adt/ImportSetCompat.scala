package vct.rewrite.adt

import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.{Generation, Rewriter}
import vct.col.rewrite.adt.{ImportADT, ImportADTBuilder, ImportADTImporter}
import vct.col.typerules.CoercingRewriter
import vct.rewrite.adt.ImportSetCompat.{
  SetEmptyFreshPreconditionFailed,
  SetEmptyPreconditionFailed,
}

case object ImportSetCompat extends ImportADTBuilder("map_compat") {
  case class SetEmptyPreconditionFailed(choose: Choose[_])
      extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      choose.blame.blame(SetEmpty(choose.xs))
  }

  case class SetEmptyFreshPreconditionFailed(choose: ChooseFresh[_])
      extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      choose.blame.blame(SetEmpty(choose.xs))
  }
}

case class ImportSetCompat[Pre <: Generation](importer: ImportADTImporter)
    extends ImportADT[Pre](importer) {
  private lazy val setCompatFile = parse("set_compat")

  private lazy val setChoose = find[Function[Post]](setCompatFile, "set_choose")
  private lazy val setChooseFresh = find[Procedure[Post]](
    setCompatFile,
    "set_choose_fresh",
  )

  override def postCoerce(e: Expr[Pre]): Expr[Post] =
    e match {
      case choose @ Choose(xs) =>
        FunctionInvocation[Post](
          ref = setChoose.ref,
          args = Seq(dispatch(xs)),
          typeArgs = Seq(dispatch(choose.t)),
          givenMap = Nil,
          yields = Nil,
        )(NoContext(SetEmptyPreconditionFailed(choose)))(e.o)

      case choose @ ChooseFresh(xs) =>
        ProcedureInvocation[Post](
          ref = setChooseFresh.ref,
          args = Seq(dispatch(xs)),
          outArgs = Nil,
          typeArgs = Seq(dispatch(choose.t)),
          givenMap = Nil,
          yields = Nil,
        )(NoContext(SetEmptyFreshPreconditionFailed(choose)))(e.o)

      case other => super.postCoerce(other)
    }
}
