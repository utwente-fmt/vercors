package vct.col.rewrite

import hre.util.FuncTools
import vct.col.ast.{Expr, _}
import vct.col.rewrite.error.ExtraNode
import vct.col.origin.{AbstractApplicable, ArraySize, ArraySizeError, ArrayValuesError, ArrayValuesFromNegative, ArrayValuesFromToOrder, ArrayValuesNull, ArrayValuesPerm, ArrayValuesToLength, Blame, ContextEverywhereFailedInPre, FailLeft, FailRight, FramedArrIndex, FramedArrLength, FramedSeqIndex, InvocationFailure, IteratedArrayInjective, NoContext, Origin, PanicBlame, PreconditionFailed, TriggerPatternBlame, TrueSatisfiable, VerificationFailure}
import vct.col.resolve.lang.Java
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.typerules.CoercionUtils
import vct.col.util.AstBuildHelpers
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.{Unreachable, UserError}

import scala.collection.mutable

case object EncodeArrayValues extends RewriterBuilder {
  override def key: String = "arrayValues"
  override def desc: String = "Encode \\values and array creation into functions/methods."

  case class ValuesFunctionOrigin(preferredName: String = "unknown") extends Origin {
    override def shortPosition: String = "generated"
    override def context: String = "[At node generated for \\values]"
    override def inlineContext: String = "[Node generated for \\values]"
  }

  case class ArrayCreationOrigin(preferredName: String = "unknown") extends Origin {
    override def shortPosition: String = "generated"
    override def context: String = "[At node generated for array creation]"
    override def inlineContext: String = "[Node generated for array creation]"
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

  case class ArrayCreationFailed(arr: NewArray[_]) extends Blame[InvocationFailure] {
    override def blame(error: InvocationFailure): Unit = error match {
      case PreconditionFailed(_, _, _) => arr.blame.blame(ArraySize(arr))
      case ContextEverywhereFailedInPre(_, _) => arr.blame.blame(ArraySize(arr)) // Unnecessary?
      case other => throw Unreachable(s"Invalid invocation failure: $other")
    }
  }

  case class PointerArrayCreationFailed(arr: NewPointerArray[_]) extends Blame[InvocationFailure] {
    override def blame(error: InvocationFailure): Unit = error match {
      case PreconditionFailed(_, _, _) => arr.blame.blame(ArraySize(arr))
      case ContextEverywhereFailedInPre(_, _) => arr.blame.blame(ArraySize(arr)) // Unnecessary?
      case other => throw Unreachable(s"Invalid invocation failure: $other")
    }
  }
}

case class EncodeArrayValues[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeArrayValues._

  val valuesFunctions: mutable.Map[Type[Pre], Function[Post]] = mutable.Map()

  val arrayCreationMethods: mutable.Map[(Type[Pre], Int, Int, Boolean), Procedure[Post]] = mutable.Map()

  val pointerArrayCreationMethods: mutable.Map[(Type[Pre]), Procedure[Post]] = mutable.Map()

  def makeFunctionFor(arrayType: TArray[Pre]): Function[Post] = {
    implicit val o: Origin = ValuesFunctionOrigin()
    val arr_var = new Variable[Post](dispatch(arrayType))(ValuesFunctionOrigin("a"))
    val from_var = new Variable[Post](TInt())(ValuesFunctionOrigin("from"))
    val to_var = new Variable[Post](TInt())(ValuesFunctionOrigin("to"))

    val arr = Local[Post](arr_var.ref)
    val from = Local[Post](from_var.ref)
    val to = Local[Post](to_var.ref)

    globalDeclarations.declare(withResult((result: Result[Post]) => function[Post](
      blame = AbstractApplicable,
      contractBlame = PanicBlame("the function for \\values always has a satisfiable contract"),
      returnType = TSeq(dispatch(arrayType.element)),
      args = Seq(arr_var, from_var, to_var),
      requires =
        SplitAccountedPredicate(UnitAccountedPredicate(arr !== Null()),
        SplitAccountedPredicate(UnitAccountedPredicate(const(0) <= from),
        SplitAccountedPredicate(UnitAccountedPredicate(from <= to),
        SplitAccountedPredicate(UnitAccountedPredicate(to <= Length(arr)(FramedArrLength)),
        UnitAccountedPredicate(starall(IteratedArrayInjective, TInt(),
          i => (from <= i && i < to) ==> Value(ArrayLocation(arr, i)(FramedArrIndex)),
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
    )))
  }

  def makeCreationMethodFor(elementType: Type[Pre], definedDims: Int, undefinedDims: Int, initialize: Boolean): Procedure[Post] = {
    implicit val o: Origin = ArrayCreationOrigin()

    val dimArgs = (0 until definedDims).map(i => new Variable[Post](TInt())(ArrayCreationOrigin(s"dim$i")))

    // ar != null
    // ar.length == dim0
    // forall ar[i] :: Perm(ar[i], write)
    //
    // forall ar[i] :: ar[i] != null
    // forall ar[i] :: ar[i].length == dim1
    // forall ar[i], ar[j] :: ar[i] == ar[j] ==> i == j
    // forall ar[i][j] :: Perm(ar[i][j], write)
    //
    // forall ar[i][j] :: ar[i][j] == null

    globalDeclarations.declare(withResult((result: Result[Post]) => {
      val forall = (count: Int,  assn: (Expr[Post], Option[ArrayLocation[Post]]) => Expr[Post]) => {
        val bindings = (0 until count).map(i => new Variable[Post](TInt())(ArrayCreationOrigin(s"i$i")))
        val access = (0 until count).foldLeft[Expr[Post]](result)((e, i) => ArraySubscript(e, bindings(i).get)(FramedArrIndex))
        val cond = foldAnd[Post](bindings.zip(dimArgs).map { case (i, dim) => const(0) <= i.get && i.get < dim.get })

        if(count == 0) assn(result, None)
        else {
          val optArrLoc = (0 until (count-1)).foldLeft[Expr[Post]](result)((e,i) => ArraySubscript(e, bindings(i).get)(FramedArrIndex))
          val location = ArrayLocation(optArrLoc, bindings(count-1).get)(FramedArrIndex)
          Starall[Post](bindings, Seq(Seq(access)), cond ==> assn(access, Some(location)))(IteratedArrayInjective)
        }
      }

      var ensures = foldStar((0 until definedDims).map(count => {
        val injective = if(count > 0) {
          val leftBindings = (0 until count).map(i => new Variable[Post](TInt())(ArrayCreationOrigin(s"i$i")))
          val rightBindings = (0 until count).map(i => new Variable[Post](TInt())(ArrayCreationOrigin(s"j$i")))

          val leftRanges = leftBindings.zip(dimArgs).map { case (i, dim) => const[Post](0) <= i.get && i.get < dim.get }
          val rightRanges = rightBindings.zip(dimArgs).map { case (i, dim) => const[Post](0) <= i.get && i.get < dim.get }

          val rangeCond = foldAnd(leftRanges) && foldAnd(rightRanges)

          val leftAccess = leftBindings.foldLeft[Expr[Post]](result)((e, b) => ArraySubscript(e, b.get)(FramedArrIndex))
          val rightAccess = rightBindings.foldLeft[Expr[Post]](result)((e, b) => ArraySubscript(e, b.get)(FramedArrIndex))

          val indicesEqual = leftBindings.zip(rightBindings).map { case (l, r) => l.get === r.get }

          Forall[Post](leftBindings ++ rightBindings, Seq(Seq(leftAccess, rightAccess)),
            rangeCond ==> ((leftAccess === rightAccess) ==> foldAnd(indicesEqual)))
        } else tt[Post]

        forall(count, (access, _) => access !== Null()) &*
          forall(count, (access, _) => Length(access)(FramedArrLength) === dimArgs(count).get) &*
          injective &*
          forall(count + 1, (_, location) => Perm(location.get, WritePerm()))
      }))

      val undefinedValue: Expr[Post] =
        dispatch(Java.zeroValue(FuncTools.repeat[Type[Pre]](TArray(_), undefinedDims, elementType)))

      ensures = if(initialize) ensures &* forall(definedDims, (access, _) => access === undefinedValue) else ensures

      val requires = foldAnd(dimArgs.map(argument => GreaterEq(argument.get, const[Post](0))))
      procedure(
        blame = AbstractApplicable,
        contractBlame = TrueSatisfiable,
        returnType = FuncTools.repeat[Type[Post]](TArray(_), definedDims + undefinedDims, dispatch(elementType)),
        args = dimArgs,
        requires = UnitAccountedPredicate(requires),
        ensures = UnitAccountedPredicate(ensures)
      )(ArrayCreationOrigin(if(initialize) "make_array_initialized" else "make_array"))
    }))
  }

  def makePointerCreationMethodFor(elementType: Type[Pre]) = {
    implicit val o: Origin = ArrayCreationOrigin()
    // ar != null
    // ar.length == dim0
    // forall ar[i] :: Perm(ar[i], write)
    val sizeArg = new Variable[Post](TInt())(ArrayCreationOrigin("size"))

    globalDeclarations.declare(withResult((result: Result[Post]) => {
      val blame = PanicBlame("Already checked")
      val requires = GreaterEq(sizeArg.get, const[Post](0))
      val binding = new Variable[Post](TInt())(ArrayCreationOrigin("i"))
      val access = PointerAdd(result, binding.get)(blame)

      val body = (const[Post](0) <= binding.get && binding.get < sizeArg.get) ==> Perm(PointerLocation(access)(blame), WritePerm())
      val forall = Starall(Seq(binding), Seq(Seq(access)), body)(blame)
      val ensures = (result !== Null()) &*
        (PointerBlockLength(result)(blame) === sizeArg.get) &*
        (PointerBlockOffset(result)(blame) === const[Post](0)) &*
        forall

      procedure(
        blame = AbstractApplicable,
        contractBlame = TrueSatisfiable,
        returnType = TPointer(dispatch(elementType)),
        args = Seq(sizeArg),
        requires = UnitAccountedPredicate(requires),
        ensures = UnitAccountedPredicate(ensures)
      )(ArrayCreationOrigin("make_pointer_array"))
    }))
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case values @ Values(arr, from, to) =>
        val arrayType = CoercionUtils.getAnyArrayCoercion(arr.t).get._2
        val func = valuesFunctions.getOrElseUpdate(arrayType, makeFunctionFor(arrayType))
        FunctionInvocation[Post](func.ref, Seq(dispatch(arr), dispatch(from), dispatch(to)), Nil, Nil, Nil)(NoContext(ArrayValuesPreconditionFailed(values)))
      case newArr @ NewArray(element, dims, moreDims, initialize) =>
        val method = arrayCreationMethods.getOrElseUpdate((element, dims.size, moreDims, initialize), makeCreationMethodFor(element, dims.size, moreDims, initialize))
        ProcedureInvocation[Post](method.ref, dims.map(dispatch), Nil, Nil, Nil, Nil)(ArrayCreationFailed(newArr))
      case newPointerArr @ NewPointerArray(element, size) =>
        val method = pointerArrayCreationMethods.getOrElseUpdate(element, makePointerCreationMethodFor(element))
        ProcedureInvocation[Post](method.ref, Seq(dispatch(size)), Nil, Nil, Nil, Nil)(PointerArrayCreationFailed(newPointerArr))
      case other => rewriteDefault(other)
    }
  }
}
