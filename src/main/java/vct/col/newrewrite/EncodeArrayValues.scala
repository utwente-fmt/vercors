package vct.col.newrewrite

import hre.util.FuncTools
import vct.col.ast._
import vct.col.coerce.CoercionUtils
import vct.col.origin.{AbstractApplicable, ArrayValuesError, ArrayValuesFromNegative, ArrayValuesFromToOrder, ArrayValuesNull, ArrayValuesPerm, ArrayValuesToLength, Blame, FailLeft, FailRight, FramedArrIndex, FramedArrLength, FramedSeqIndex, NoContext, Origin, PanicBlame, PreconditionFailed, TriggerPatternBlame}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationResult.Unreachable

import scala.collection.mutable

case object EncodeArrayValues extends RewriterBuilder {
  case class ValuesFunctionOrigin(preferredName: String = "unknown") extends Origin {
    override def context: String = "[At node generated for \\values]"
  }

  case class ArrayCreationOrigin(preferredName: String = "unknown") extends Origin {
    override def context: String = "[At node generated for array creation]"
  }

  case class ArrayValuesPreconditionFailed(values: Values[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit = error.path match {
      case Seq(FailLeft) =>
        values.blame.blame(ArrayValuesNull(values))
      case Seq(FailRight, FailLeft) =>
        values.blame.blame(ArrayValuesFromNegative(values))
      case Seq(FailRight, FailRight, FailLeft) =>
        values.blame.blame(ArrayValuesFromToOrder(values))
      case Seq(FailRight, FailRight, FailRight, FailLeft) =>
        values.blame.blame(ArrayValuesToLength(values))
      case Seq(FailRight, FailRight, FailRight, FailRight) =>
        values.blame.blame(ArrayValuesPerm(values))
      case other => throw Unreachable(s"Invalid postcondition path sequence: $other")
    }
  }
}

case class EncodeArrayValues[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeArrayValues._

  val valuesFunctions: mutable.Map[Type[Pre], Function[Post]] = mutable.Map()

  val arrayCreationMethods: mutable.Map[(Type[Pre], Int, Int), Procedure[Post]] = mutable.Map()

  def makeFunctionFor(arrayType: TArray[Pre]): Function[Post] = {
    implicit val o: Origin = ValuesFunctionOrigin()
    val arr_var = new Variable[Post](dispatch(arrayType))(ValuesFunctionOrigin("a"))
    val from_var = new Variable[Post](TInt())(ValuesFunctionOrigin("from"))
    val to_var = new Variable[Post](TInt())(ValuesFunctionOrigin("to"))

    val arr = Local[Post](arr_var.ref)
    val from = Local[Post](from_var.ref)
    val to = Local[Post](to_var.ref)

    val f = withResult((result: Result[Post]) => function[Post](
      blame = AbstractApplicable,
      returnType = TSeq(dispatch(arrayType.element)),
      args = Seq(arr_var, from_var, to_var),
      requires =
        SplitAccountedPredicate(UnitAccountedPredicate(arr !== Null()),
        SplitAccountedPredicate(UnitAccountedPredicate(const(0) <= from),
        SplitAccountedPredicate(UnitAccountedPredicate(from <= to),
        SplitAccountedPredicate(UnitAccountedPredicate(to < Length(arr)(FramedArrLength)),
        UnitAccountedPredicate(starall(TInt(),
          i => (from <= i && i < to) ==> Perm(ArraySubscript(arr, i)(FramedArrIndex), ReadPerm()),
          i => Seq(Seq(ArraySubscript(arr, i)(TriggerPatternBlame))),
        )))))),
      ensures = UnitAccountedPredicate(
        (Size(result) === to - from) &&
        forall(TInt(),
          i => (const(0) <= i && i < to - from) ==> (SeqSubscript(result, i)(FramedSeqIndex) === ArraySubscript(arr, i + from)(FramedArrIndex)),
          i => Seq(Seq(SeqSubscript(result, i)(TriggerPatternBlame)))
        ) &* forall(TInt(),
          i => (from <= i && i < to) ==> (ArraySubscript(arr, i)(FramedArrIndex) === SeqSubscript(result, i - from)(FramedSeqIndex)),
          i => Seq(Seq(ArraySubscript(arr, i)(TriggerPatternBlame)))
        )
      )
    ))
    f.declareDefault(this)
    f
  }

  def makeCreationMethodFor(elementType: Type[Pre], definedDims: Int, undefinedDims: Int): Procedure[Post] = {
    implicit val o: Origin = ArrayCreationOrigin()

    val dimArgs = (0 until definedDims).map(i => new Variable[Post](TInt())(ArrayCreationOrigin(s"dim$i")))

    // ar != null
    // ar.length == dim0
    // forall ar[i] :: Perm(ar[i], write)
    //
    // forall ar[i] :: ar[i] != null
    // forall ar[i] :: ar[i].length == dim1
    // forall ar[i][j] :: Perm(ar[i][j], write)
    //
    // forall ar[i][j] :: ar[i][j] == null

    val p = withResult((result: Result[Post]) => {
      val forall = (count: Int, assn: Expr[Post] => Expr[Post]) => {
        val bindings = (0 until count).map(i => new Variable[Post](TInt())(ArrayCreationOrigin(s"i$i")))
        val access = (0 until count).foldLeft[Expr[Post]](result)((e, i) => ArraySubscript(e, bindings(i).get)(FramedArrIndex))
        val cond = foldAnd[Post](bindings.zip(dimArgs).map { case (i, dim) => const(0) <= i.get && i.get < dim.get })

        if(count == 0) assn(result)
        else Starall[Post](bindings, Seq(Seq(access)), cond ==> assn(access))
      }

      val ensures = foldStar((0 until definedDims).map(count =>
        forall(count, access => access !== Null()) &*
          forall(count, access => Length(access)(FramedArrLength) === dimArgs(count).get) &*
          forall(count + 1, access => Perm(access, WritePerm()))
      ))

      procedure(
        blame = AbstractApplicable,
        returnType = FuncTools.repeat[Type[Post]](TArray(_), definedDims + undefinedDims, dispatch(elementType)),
        args = dimArgs,
        ensures = UnitAccountedPredicate(ensures &* forall(definedDims, access => access === Null()))
      )(ArrayCreationOrigin("make_array"))
    })
    p.declareDefault(this)
    p
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case values @ Values(arr, from, to) =>
        val arrayType = CoercionUtils.getAnyArrayCoercion(arr.t).get._2
        val func = valuesFunctions.getOrElseUpdate(arrayType, makeFunctionFor(arrayType))
        FunctionInvocation[Post](func.ref, Seq(dispatch(arr), dispatch(from), dispatch(to)), Nil, Nil, Nil)(NoContext(ArrayValuesPreconditionFailed(values)))
      case NewArray(element, dims, moreDims) =>
        val method = arrayCreationMethods.getOrElseUpdate((element, dims.size, moreDims), makeCreationMethodFor(element, dims.size, moreDims))
        ProcedureInvocation[Post](method.ref, dims.map(dispatch), Nil, Nil, Nil, Nil)(PanicBlame("Array creation requires nothing."))
      case other => rewriteDefault(other)
    }
  }
}
