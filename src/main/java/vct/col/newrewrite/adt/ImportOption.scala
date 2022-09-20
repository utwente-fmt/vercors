package vct.col.newrewrite.adt

import vct.col.ast._
import vct.col.origin.{Blame, NoContext, OptionNone, Origin, PanicBlame, PreconditionFailed}
import vct.col.rewrite.Generation

case object ImportOption extends ImportADTBuilder("option") {
  case class OptionNonePreconditionFailed(access: OptGet[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      access.blame.blame(OptionNone(access))
  }
}

case class ImportOption[Pre <: Generation](importer: ImportADTImporter) extends AImportADT[Pre](importer) {
  import ImportOption._

  private lazy val optionFile = parse("option")

  private lazy val optionAdt = find[AxiomaticDataType[Post]](optionFile, "option")
  private lazy val optionNone = find[ADTFunction[Post]](optionAdt, "None")
  private lazy val optionSome = find[ADTFunction[Post]](optionAdt, "some")
  private lazy val optionAxGet = find[ADTFunction[Post]](optionAdt, "option_get")
  private lazy val optionGet = find[Function[Post]](optionFile, "opt_get")
  private lazy val optionGetOrElse = find[Function[Post]](optionFile, "opt_or_else")

  def optNone(t: Type[Post])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation[Post](
      Some((optionAdt.ref, Seq(t))),
      optionNone.ref, Nil,
    )

  def optSome(e: Expr[Post], t: Type[Post])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation[Post](
      Some((optionAdt.ref, Seq(t))),
      optionSome.ref, Seq(e),
    )

  def optGet(e: Expr[Post], t: Type[Post], blame: Blame[PreconditionFailed])(implicit o: Origin): Expr[Post] =
    FunctionInvocation[Post](optionGet.ref, Seq(e), Seq(t), Nil, Nil)(NoContext(blame))

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TOption(element) => TAxiomatic(optionAdt.ref, Seq(dispatch(element)))
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case OptNone() =>
      optNone(TNothing())(e.o)
    case OptSome(element) =>
      optSome(dispatch(element), dispatch(element.t))(e.o)
    case access@OptGet(opt) =>
      optGet(dispatch(opt), dispatch(opt.t.asOption.get.element), OptionNonePreconditionFailed(access))(e.o)
    case get@OptGetOrElse(opt, alt) =>
      FunctionInvocation[Post](optionGetOrElse.ref,
        Seq(dispatch(opt), dispatch(alt)),
        Seq(dispatch(get.t)), Nil, Nil,
      )(PanicBlame("opt_or_else requires nothing."))(e.o)
    case other => rewriteDefault(other)
  }
}
