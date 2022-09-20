package vct.col.newrewrite.adt

import vct.col.ast._
import vct.col.origin.{Blame, NoContext, NotLeft, NotRight, Origin, PreconditionFailed}
import vct.col.rewrite.Generation

case object ImportEither extends ImportADTBuilder("either") {
  case class NotLeftPreconditionFailed(get: GetLeft[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      get.blame.blame(NotLeft(get))
  }

  case class NotRightPreconditionFailed(get: GetRight[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      get.blame.blame(NotRight(get))
  }
}

case class ImportEither[Pre <: Generation](importer: ImportADTImporter) extends AImportADT[Pre](importer) {
  import ImportEither._

  private lazy val eitherFile = parse("either")

  private lazy val eitherAdt = find[AxiomaticDataType[Post]](eitherFile, "either")
  private lazy val eitherLeft = find[ADTFunction[Post]](eitherAdt, "left")
  private lazy val eitherRight = find[ADTFunction[Post]](eitherAdt, "right")
  private lazy val eitherIsRight = find[ADTFunction[Post]](eitherAdt, "is_right")
  private lazy val eitherAxGetLeft = find[ADTFunction[Post]](eitherAdt, "either_get_left")
  private lazy val eitherAxGetRight = find[ADTFunction[Post]](eitherAdt, "either_get_right")
  private lazy val eitherGetLeft = find[Function[Post]](eitherFile, "get_left")
  private lazy val eitherGetRight = find[Function[Post]](eitherFile, "get_right")

  def eitherIsRight(e: Expr[Post], leftType: Type[Post], rightType: Type[Post])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation(
      Some((
        eitherAdt.ref,
        Seq(leftType, rightType)
      )),
      eitherIsRight.ref,
      Seq(e),
    )

  def eitherGetLeft(e: Expr[Post], leftType: Type[Post], rightType: Type[Post], blame: Blame[PreconditionFailed])(implicit o: Origin): Expr[Post] =
    FunctionInvocation[Post](eitherGetLeft.ref,
      Seq(e),
      Seq(leftType, rightType), Nil, Nil,
    )(NoContext(blame))

  def eitherGetRight(e: Expr[Post], leftType: Type[Post], rightType: Type[Post], blame: Blame[PreconditionFailed])(implicit o: Origin): Expr[Post] =
    FunctionInvocation[Post](eitherGetRight.ref,
      Seq(e),
      Seq(leftType, rightType), Nil, Nil,
    )(NoContext(blame))

  def eitherLeft(e: Expr[Post], leftType: Type[Post], rightType: Type[Post])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation[Post](
      Some((
        eitherAdt.ref,
        Seq(leftType, rightType),
      )),
      eitherLeft.ref,
      Seq(e),
    )

  def eitherRight(e: Expr[Post], leftType: Type[Post], rightType: Type[Post])(implicit o: Origin): Expr[Post] =
    ADTFunctionInvocation[Post](
      Some((
        eitherAdt.ref,
        Seq(leftType, rightType),
      )),
      eitherRight.ref,
      Seq(e),
    )

  override def dispatch(t: Type[Pre]): Type[Post] = t match {
    case TEither(left, right) => TAxiomatic(eitherAdt.ref, Seq(dispatch(left), dispatch(right)))
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case EitherLeft(e) =>
        eitherLeft(dispatch(e), dispatch(e.t), dispatch(TNothing()))
      case EitherRight(e) =>
        eitherRight(dispatch(e), dispatch(TNothing()), dispatch(e.t))
      case get@GetLeft(e) =>
        eitherGetLeft(dispatch(e), dispatch(get.eitherType.left), dispatch(get.eitherType.right), NotLeftPreconditionFailed(get))
      case get@GetRight(e) =>
        eitherGetRight(dispatch(e), dispatch(get.eitherType.left), dispatch(get.eitherType.right), NotRightPreconditionFailed(get))
      case is@IsLeft(e) =>
        Not(eitherIsRight(dispatch(e), dispatch(is.eitherType.left), dispatch(is.eitherType.right)))
      case is@IsRight(e) =>
        eitherIsRight(dispatch(e), dispatch(is.eitherType.left), dispatch(is.eitherType.right))
      case other => rewriteDefault(other)
    }
  }
}
