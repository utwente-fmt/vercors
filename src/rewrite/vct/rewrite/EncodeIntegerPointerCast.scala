package vct.rewrite

import vct.col.ast.{
  Expr,
  Function,
  FunctionInvocation,
  IntegerPointerCast,
  Null,
  PointerAddress,
  PointerCast,
  Result,
  Select,
  TAxiomatic,
  TBoundedInt,
  TInt,
  TNonNullPointer,
  TPointer,
  TVoid,
  ToNonNull,
  UnitAccountedPredicate,
  Variable,
}
import vct.col.origin.{
  AbstractApplicable,
  LabelContext,
  NonNullPointerNull,
  Origin,
  PanicBlame,
  TrueSatisfiable,
}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._

case object EncodeIntegerPointerCast extends RewriterBuilder {
  override def key: String = "integerPointerCast"

  override def desc: String = "Encodes the integer-pointer casts (both ways)"

  private val CastOrigin: Origin = Origin(
    Seq(LabelContext("IntegerPointerCast helper functions"))
  )
}

case class EncodeIntegerPointerCast[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeIntegerPointerCast._

  private lazy val pointerFromAddress: Function[Post] = {
    implicit val o: Origin = CastOrigin
    val address = new Variable[Post](TInt())(o.where(name = "address"))
    globalDeclarations.declare(withResult((result: Result[Post]) =>
      function[Post](
        AbstractApplicable,
        TrueSatisfiable,
        TNonNullPointer(TVoid(), None),
        args = Seq(address),
        ensures = UnitAccountedPredicate(
          PointerAddress(result, const(1))(NonNullPointerNull) === address.get
        ),
      )(o.where(name = "pointer_from_address"))
    ))
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o;
    e match {
      case IntegerPointerCast(value, targetType, typeSize) =>
        val newValue = dispatch(value)
        val newFromType = dispatch(value.t)
        val newToType = dispatch(targetType)
        val newSize = dispatch(typeSize)
        (targetType, value.t) match {
          case (TInt() | TBoundedInt(_, _), TPointer(_, unique)) =>
            letIfNonTrivial(
              newFromType,
              newValue,
              { v =>
                Select[Post](
                  v === Null(),
                  const(0),
                  PointerAddress(
                    PointerCast(
                      ToNonNull(v)(PanicBlame(
                        "Cannot be null since this was ensured in the conditional"
                      )),
                      TNonNullPointer(TVoid(), unique),
                      newSize,
                      const(1),
                    ),
                    const(1),
                  )(NonNullPointerNull),
                )
              },
            )
          case (TInt() | TBoundedInt(_, _), TNonNullPointer(_, unique)) =>
            PointerAddress(
              PointerCast(
                dispatch(value),
                TNonNullPointer(TVoid(), unique),
                newSize,
                const(1),
              ),
              const(1),
            )(NonNullPointerNull)
          case (TPointer(_, None), TInt() | TBoundedInt(_, _)) =>
            letIfNonTrivial(
              newFromType,
              newValue,
              { v =>
                Select[Post](
                  v === const(0),
                  Null(),
                  PointerCast(
                    functionInvocation[Post](
                      TrueSatisfiable,
                      pointerFromAddress.ref,
                      args = Seq(v),
                    ),
                    newToType,
                    const(1),
                    newSize,
                  ),
                )
              },
            )
          case (TNonNullPointer(_, None), TInt() | TBoundedInt(_, _)) =>
            PointerCast(
              functionInvocation[Post](
                TrueSatisfiable,
                pointerFromAddress.ref,
                args = Seq(newValue),
              ),
              newToType,
              const(1),
              newSize,
            )
        }
      case _ => super.dispatch(e)
    }
  }
}
