package vct.col.newrewrite

import vct.col.ast._
import vct.col.coerce.Coercion
import vct.col.origin.{AbstractApplicable, ArrayValuesError, Blame, FramedArrIndex, FramedArrLength, FramedSeqIndex, Origin, PreconditionFailed, TriggerPatternBlame}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable

case object EncodeArrayValues extends RewriterBuilder {
  case class ValuesFunctionOrigin(preferredName: String = "unknown") extends Origin {
    override def messageInContext(message: String): String =
      s"[At node generated for \\values]: $message"
  }

  case class ArrayValuesPreconditionFailed(values: Values[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      values.blame.blame(ArrayValuesError(values))
  }
}

case class EncodeArrayValues[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeArrayValues._

  val valuesFunctions: mutable.Map[Type[Pre], Function[Post]] = mutable.Map()

  def makeFunctionFor(arrayType: TArray[Pre]): Function[Post] = {
    implicit val o: Origin = ValuesFunctionOrigin()
    val arr_var = new Variable[Post](dispatch(arrayType))(ValuesFunctionOrigin("a"))
    val from_var = new Variable[Post](TInt())(ValuesFunctionOrigin("from"))
    val to_var = new Variable[Post](TInt())(ValuesFunctionOrigin("to"))

    val arr = Local[Post](arr_var.ref)
    val from = Local[Post](from_var.ref)
    val to = Local[Post](to_var.ref)

    withResult((result: Result[Post]) => function[Post](
      blame = AbstractApplicable,
      returnType = TSeq(dispatch(arrayType.element)),
      args = Seq(arr_var, from_var, to_var),
      requires =
        arr !== Null() &*
        const(0) <= from &* from <= to &* to < Length(arr)(FramedArrLength) &*
        starall(TInt(),
          i => (from <= i && i < to) ==> Perm(ArraySubscript(arr, i)(FramedArrIndex), ReadPerm()),
          i => Seq(Seq(ArraySubscript(arr, i)(TriggerPatternBlame))),
        ),
      ensures =
        Size(result) === to - from &&
        forall(TInt(),
          i => (const(0) <= i && i < to - from) ==> SeqSubscript(result, i)(FramedSeqIndex) === ArraySubscript(arr, i + from)(FramedArrIndex),
          i => Seq(Seq(SeqSubscript(result, i)(TriggerPatternBlame)))
        ) &* forall(TInt(),
          i => (from <= i && i < to) ==> ArraySubscript(arr, i)(FramedArrIndex) === SeqSubscript(result, i - from)(FramedSeqIndex),
          i => Seq(Seq(ArraySubscript(arr, i)(TriggerPatternBlame)))
        )
    ))
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = ValuesFunctionOrigin()
    e match {
      case values @ Values(arr, from, to) =>
        val arrayType = Coercion.getAnyArrayCoercion(arr.t).get._2
        val func = valuesFunctions.getOrElseUpdate(arrayType, makeFunctionFor(arrayType))
        FunctionInvocation[Post](func.ref, Seq(dispatch(arr), dispatch(from), dispatch(to)), Nil)(ArrayValuesPreconditionFailed(values))
      case other => rewriteDefault(other)
    }
  }
}
