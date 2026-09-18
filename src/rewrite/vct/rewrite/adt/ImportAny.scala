package vct.col.rewrite.adt

import hre.util.ScopedStack
import vct.col.ast.{
  AxiomaticDataType,
  Cast,
  CoerceSomethingAnyValue,
  Coercion,
  Expr,
  Function,
  FunctionInvocation,
  TAnyValue,
  TAxiomatic,
  TType,
  Type,
  TypeValue,
}
import vct.col.origin.{Origin, PanicBlame}
import vct.col.ref.LazyRef
import vct.col.rewrite.Generation

case object ImportAny extends ImportADTBuilder("any")

case class ImportAny[Pre <: Generation](importer: ImportADTImporter)
    extends ImportADT[Pre](importer) {
  val inAnyLoad: ScopedStack[Unit] = ScopedStack()

  private lazy val anyFile = inAnyLoad.having(()) { parse("any") }

  private lazy val anyAdt = find[AxiomaticDataType[Post]](anyFile, "any")
  private lazy val anyFrom = find[Function[Post]](anyFile, "as_any")

  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(
      implicit o: Origin
  ): Expr[Post] =
    coercion match {
      case CoerceSomethingAnyValue(source) =>
        FunctionInvocation[Post](
          anyFrom.ref,
          Seq(e),
          Seq(dispatch(source)),
          Nil,
          Nil,
        )(PanicBlame("coercing to any requires nothing."))
      case other => super.applyCoercion(e, other)
    }

  override def postCoerce(t: Type[Pre]): Type[Post] =
    t match {
      case TType(TAnyValue()) =>
        // Only the any adt definition refers to itself, so this is the only place this trick is necessary.
        if (inAnyLoad.isEmpty)
          super.postCoerce(t)
        else
          TType(TAxiomatic(new LazyRef(anyAdt), Nil))
      case TAnyValue() => TAxiomatic(anyAdt.ref, Nil)
      case other => super.postCoerce(other)
    }

  override def postCoerce(e: Expr[Pre]): Expr[Post] =
    e match {
      case Cast(value, TypeValue(TAnyValue())) =>
        FunctionInvocation[Post](
          anyFrom.ref,
          Seq(dispatch(value)),
          Seq(dispatch(value.t)),
          Nil,
          Nil,
        )(PanicBlame("coercing to any requires nothing."))(e.o)
      case other => super.postCoerce(other)
    }
}
