package vct.col.rewrite

import hre.util.FuncTools
import vct.col.ast.{Expr, _}
import vct.col.origin._
import vct.col.resolve.ctx.Referrable
import vct.col.resolve.lang.Java
import vct.rewrite.lang.LangCToCol.UnsupportedStructPerm
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.typerules.CoercionUtils
import vct.col.util.AstBuildHelpers
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.{Unreachable, UserError}

import scala.collection.mutable

case object EncodeArrayValues extends RewriterBuilder {
  override def key: String = "arrayValues"
  override def desc: String =
    "Encode \\values and array creation into functions/methods."

  private val valuesFunctionOrigin: Origin = Origin(
    Seq(LabelContext("\\values function"))
  )

  private val arrayCreationOrigin: Origin = Origin(
    Seq(LabelContext("array creation method"))
  )

  private val freeFuncOrigin: Origin = Origin(
    Seq(LabelContext("pointer free method"))
  )

  case class ArrayValuesPreconditionFailed(values: Values[_])
      extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      error.path match {
        case Seq(FailLeft) => values.blame.blame(ArrayValuesNull(values))
        case Seq(FailRight, FailLeft) =>
          values.blame.blame(ArrayValuesFromNegative(values))
        case Seq(FailRight, FailRight, FailLeft) =>
          values.blame.blame(ArrayValuesFromToOrder(values))
        case Seq(FailRight, FailRight, FailRight, FailLeft) =>
          values.blame.blame(ArrayValuesToLength(values))
        case Seq(FailRight, FailRight, FailRight, FailRight) =>
          values.blame.blame(ArrayValuesPerm(values))
        case other =>
          throw Unreachable(s"Invalid postcondition path sequence: $other")
      }
  }

  case class ArrayCreationFailed(arr: NewArray[_])
      extends Blame[InvocationFailure] {
    override def blame(error: InvocationFailure): Unit =
      error match {
        case PreconditionFailed(_, _, _) => arr.blame.blame(ArraySize(arr))
        case ContextEverywhereFailedInPre(_, _) =>
          arr.blame.blame(ArraySize(arr)) // Unnecessary?
        case other => throw Unreachable(s"Invalid invocation failure: $other")
      }
  }

  case class PointerArrayCreationFailed(
      arr: Expr[_],
      blame: Blame[ArraySizeError],
  ) extends Blame[InvocationFailure] {
    override def blame(error: InvocationFailure): Unit =
      error match {
        case PreconditionFailed(_, _, _) => blame.blame(ArraySize(arr))
        case ContextEverywhereFailedInPre(_, _) =>
          blame.blame(ArraySize(arr)) // Unnecessary?
        case other => throw Unreachable(s"Invalid invocation failure: $other")
      }
  }

  case class PointerFreeFailed[G](
      free: FreePointer[G],
      errors: Seq[Expr[G] => PointerFreeError],
  ) extends Blame[InvocationFailure] {
    override def blame(error: InvocationFailure): Unit =
      error match {
        case PreconditionFailed(path, _, _) => blame_searcher(path, errors)
        case other => throw Unreachable(s"Invalid invocation failure: $other")
      }

    def blame_searcher(
        path: Seq[AccountedDirection],
        errors: Seq[Expr[G] => PointerFreeError],
    ): Unit =
      (path, errors) match {
        case (Seq(FailLeft), e :: _) => free.blame.blame(e(free.pointer))
        case (Seq(FailRight), _ :: e :: _) => free.blame.blame(e(free.pointer))
        case (FailRight :: pathTail, _ :: errorsTail) =>
          blame_searcher(pathTail, errorsTail)
        case _ => throw Unreachable(s"Invalid invocation failure for free")
      }
  }
}

case class EncodeArrayValues[Pre <: Generation]() extends Rewriter[Pre] {
  import EncodeArrayValues._

  val valuesFunctions: mutable.Map[Type[Pre], Function[Post]] = mutable.Map()

  val arrayCreationMethods
      : mutable.Map[(Type[Pre], Int, Int, Boolean), Procedure[Post]] = mutable
    .Map()

  val pointerArrayCreationMethods: mutable.Map[Type[Pre], Procedure[Post]] =
    mutable.Map()
  val nonNullPointerArrayCreationMethods
      : mutable.Map[Type[Pre], Procedure[Post]] = mutable.Map()

  val freeMethods: mutable.Map[Type[
    Post
  ], (Procedure[Post], FreePointer[Pre] => PointerFreeFailed[Pre])] = mutable
    .Map()

  def makeFree(
      t: Type[Post]
  ): (Procedure[Post], FreePointer[Pre] => PointerFreeFailed[Pre]) = {
    implicit val o: Origin = freeFuncOrigin
    var errors: Seq[Expr[Pre] => PointerFreeError] = Seq()

    val proc = globalDeclarations.declare({
      val (vars, ptr) = variables.collect {
        val a_var = new Variable[Post](TPointer(t))(o.where(name = "p"))
        variables.declare(a_var)
        Local[Post](a_var.ref)
      }
      val zero = const[Post](0)
      val size = PointerBlockLength(ptr)(FramedPtrBlockLength)

      val i = new Variable[Post](TInt())(o.where(name = "i"))
      val j = new Variable[Post](TInt())(o.where(name = "j"))
      val access =
        (i: Variable[Post]) => PointerSubscript(ptr, i.get)(FramedPtrOffset)

      val makeStruct = MakeAnns(
        i,
        j,
        size,
        access(i),
        Seq(access(i), access(j)),
      )

      /*@
        requires ptr != NULL;
        requires \pointer_block_offset(ptr) == 0;
        requires (\forall* int i; 0 <= i && i < \pointer_block_length(ptr); Perm(&ptr[i], write));
        (if ptr is struct type):
        requires (\forall int i, j; 0 <= i,j && i,j < \pointer_block_length(ptr); i!=j ==> &ptr[i] != &ptr[j]);
        requires (\forall* int i; 0 <= i && i < \pointer_block_length(ptr); Perm(ptr[i], write));
        (and recurse for struct fields)
       */
      var requiresT: Seq[(Expr[Post], Expr[Pre] => PointerFreeError)] = Seq(
        (ptr !== Null(), (p: Expr[Pre]) => PointerNull(p)),
        (
          PointerBlockOffset(ptr)(FramedPtrBlockOffset) === zero,
          (p: Expr[Pre]) => PointerOffsetNonZero(p),
        ),
        (
          makeStruct.makePerm(
            i =>
              PointerLocation(PointerAdd(ptr, i.get)(FramedPtrOffset))(
                FramedPtrOffset
              ),
            IteratedPtrInjective,
          ),
          (p: Expr[Pre]) => PointerInsufficientFreePermission(p),
        ),
      )
      var requires = (ptr !== Null()) &*
        (PointerBlockOffset(ptr)(FramedPtrBlockOffset) === zero) &*
        makeStruct.makePerm(
          i =>
            PointerLocation(PointerAdd(ptr, i.get)(FramedPtrOffset))(
              FramedPtrOffset
            ),
          IteratedPtrInjective,
        )
      requiresT =
        if (!typeIsRef(t))
          requiresT
        else {
          // I think this error actually can never happen, since we require full write permission already
          requiresT :+
            (
              makeStruct.makeUnique(access),
              (p: Expr[Pre]) => GenericPointerFreeError(p),
            )
        }
      // If structure contains structs, the permission for those fields need to be released as well
      val permFields =
        t match {
//          case t: TClass[Post] => unwrapStructPerm(access, t, o, makeStruct)
          case _ => Seq()
        }
      requiresT =
        if (permFields.isEmpty)
          requiresT
        else
          requiresT ++ permFields
      val requiresPred = foldPredicate(requiresT.map(_._1))
      errors = requiresT.map(_._2)

      procedure(
        blame = AbstractApplicable,
        contractBlame = TrueSatisfiable,
        returnType = TVoid[Post](),
        args = vars,
        outArgs = Nil,
        typeArgs = Nil,
        body = None,
        requires = requiresPred,
        decreases = Some(DecreasesClauseNoRecursion[Post]()),
      )(o.where("free_" + t.toString))
    })
    (proc, (node: FreePointer[Pre]) => PointerFreeFailed(node, errors))
  }

  def makeFunctionFor(arrayType: TArray[Pre]): Function[Post] = {
    implicit val o: Origin = valuesFunctionOrigin
    val arr_var = new Variable[Post](dispatch(arrayType))(o.where(name = "a"))
    val from_var = new Variable[Post](TInt())(o.where(name = "from"))
    val to_var = new Variable[Post](TInt())(o.where(name = "to"))

    val arr = Local[Post](arr_var.ref)
    val from = Local[Post](from_var.ref)
    val to = Local[Post](to_var.ref)

    globalDeclarations.declare(withResult((result: Result[Post]) =>
      function[Post](
        blame = AbstractApplicable,
        contractBlame = PanicBlame(
          "the function for \\values always has a satisfiable contract"
        ),
        returnType = TSeq(dispatch(arrayType.element)),
        args = Seq(arr_var, from_var, to_var),
        requires = SplitAccountedPredicate(
          UnitAccountedPredicate(arr !== Null()),
          SplitAccountedPredicate(
            UnitAccountedPredicate(const(0) <= from),
            SplitAccountedPredicate(
              UnitAccountedPredicate(from <= to),
              SplitAccountedPredicate(
                UnitAccountedPredicate(to <= Length(arr)(FramedArrLength)),
                UnitAccountedPredicate(starall(
                  IteratedArrayInjective,
                  TInt(),
                  i =>
                    (from <= i && i < to) ==>
                      Value(ArrayLocation(arr, i)(FramedArrIndex)),
                  i => Seq(Seq(ArraySubscript(arr, i)(TriggerPatternBlame))),
                )),
              ),
            ),
          ),
        ),
        ensures = UnitAccountedPredicate(
          (Size(result) === to - from) && forall(
            TInt(),
            i =>
              (const(0) <= i && i < to - from) ==>
                (SeqSubscript(result, i)(FramedSeqIndex) ===
                  ArraySubscript(arr, i + from)(FramedArrIndex)),
            i => Seq(Seq(SeqSubscript(result, i)(TriggerPatternBlame))),
          ) &* forall(
            TInt(),
            i =>
              (from <= i && i < to) ==>
                (ArraySubscript(arr, i)(FramedArrIndex) ===
                  SeqSubscript(result, i - from)(FramedSeqIndex)),
            i => Seq(Seq(ArraySubscript(arr, i)(TriggerPatternBlame))),
          )
        ),
      )(o.where(name = "values"))
    ))
  }

  def makeCreationMethodFor(
      elementType: Type[Pre],
      definedDims: Int,
      undefinedDims: Int,
      initialize: Boolean,
  ): Procedure[Post] = {
    implicit val o: Origin = arrayCreationOrigin

    val dimArgs = (0 until definedDims)
      .map(i => new Variable[Post](TInt())(o.where(name = s"dim$i")))

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
      val forall =
        (
            count: Int,
            assn: (Expr[Post], Option[ArrayLocation[Post]]) => Expr[Post],
        ) => {
          val bindings = (0 until count)
            .map(i => new Variable[Post](TInt())(o.where(name = s"i$i")))
          val access = (0 until count).foldLeft[Expr[Post]](result)((e, i) =>
            ArraySubscript(e, bindings(i).get)(FramedArrIndex)
          )
          val cond = foldAnd[Post](bindings.zip(dimArgs).map { case (i, dim) =>
            const(0) <= i.get && i.get < dim.get
          })

          if (count == 0)
            assn(result, None)
          else {
            val optArrLoc = (0 until (count - 1))
              .foldLeft[Expr[Post]](result)((e, i) =>
                ArraySubscript(e, bindings(i).get)(FramedArrIndex)
              )
            val location =
              ArrayLocation(optArrLoc, bindings(count - 1).get)(FramedArrIndex)
            Starall[Post](
              bindings,
              Seq(Seq(access)),
              cond ==> assn(access, Some(location)),
            )(IteratedArrayInjective)
          }
        }

      var ensures = foldStar((0 until definedDims).map(count => {
        val injective =
          if (count > 0) {
            val leftBindings = (0 until count)
              .map(i => new Variable[Post](TInt())(o.where(name = s"i$i")))
            val rightBindings = (0 until count)
              .map(i => new Variable[Post](TInt())(o.where(name = s"j$i")))

            val leftRanges = leftBindings.zip(dimArgs).map { case (i, dim) =>
              const[Post](0) <= i.get && i.get < dim.get
            }
            val rightRanges = rightBindings.zip(dimArgs).map { case (i, dim) =>
              const[Post](0) <= i.get && i.get < dim.get
            }

            val rangeCond = foldAnd(leftRanges) && foldAnd(rightRanges)

            val leftAccess =
              leftBindings.foldLeft[Expr[Post]](result)((e, b) =>
                ArraySubscript(e, b.get)(FramedArrIndex)
              )
            val rightAccess =
              rightBindings.foldLeft[Expr[Post]](result)((e, b) =>
                ArraySubscript(e, b.get)(FramedArrIndex)
              )

            val indicesEqual = leftBindings.zip(rightBindings).map {
              case (l, r) => l.get === r.get
            }

            Forall[Post](
              leftBindings ++ rightBindings,
              Seq(Seq(leftAccess, rightAccess)),
              rangeCond ==>
                ((leftAccess === rightAccess) ==> foldAnd(indicesEqual)),
            )
          } else
            tt[Post]

        forall(count, (access, _) => access !== Null()) &* forall(
          count,
          (access, _) => Length(access)(FramedArrLength) === dimArgs(count).get,
        ) &* injective &*
          forall(count + 1, (_, location) => Perm(location.get, WritePerm()))
      }))

      val undefinedValue: Expr[Post] = dispatch(Java.zeroValue(
        FuncTools.repeat[Type[Pre]](TArray(_), undefinedDims, elementType)
      ))

      ensures =
        if (initialize)
          ensures &*
            forall(definedDims, (access, _) => access === undefinedValue)
        else
          ensures

      val requires = foldAnd(
        dimArgs.map(argument => GreaterEq(argument.get, const[Post](0)))
      )
      procedure(
        blame = AbstractApplicable,
        contractBlame = TrueSatisfiable,
        returnType = FuncTools.repeat[Type[Post]](
          TArray(_),
          definedDims + undefinedDims,
          dispatch(elementType),
        ),
        args = dimArgs,
        requires = UnitAccountedPredicate(requires),
        ensures = UnitAccountedPredicate(ensures),
      )(o.where(name =
        if (initialize)
          "make_array_initialized"
        else
          "make_array"
      ))
    }))
  }

  def unwrapStructPerm(
      struct: Variable[Post] => Expr[Post],
      structType: TClass[Post],
      origin: Origin,
      makeStruct: MakeAnns,
      visited: Seq[TClass[Post]] = Seq(),
  ): Seq[(Expr[Post], Expr[Pre] => PointerFreeError)] = {
    if (visited.contains(structType))
      throw UnsupportedStructPerm(
        origin
      ) // We do not allow this notation for recursive structs
    implicit val o: Origin = origin

    // TODO: Instead of doing complicated stuff here just generate a Perm(struct.field, write) and rely on EncodyByValueClass to deal with it :)
    val fields =
      structType match {
        case t: TClass[Post] =>
          t.cls.decl.declarations.collect { case field: InstanceField[Post] =>
            field
          }
        case _ => Seq()
      }
    val newFieldPerms = fields.map(member => {
      val loc =
        (i: Variable[Post]) =>
          DerefPointer(Deref[Post](struct(i), member.ref)(DerefPerm))(
            NonNullPointerNull
          )
      var anns: Seq[(Expr[Post], Expr[Pre] => PointerFreeError)] = Seq((
        makeStruct.makePerm(
          i => FieldLocation[Post](struct(i), member.ref),
          IteratedPtrInjective,
        ),
        (p: Expr[Pre]) =>
          PointerInsufficientFreeFieldPermission(
            p,
            Referrable.originName(member),
          ),
      ))
      anns =
        if (typeIsRef(member.t.asPointer.get.element))
          anns :+
            (
              makeStruct.makeUnique(loc),
              (p: Expr[Pre]) => GenericPointerFreeError(p),
            )
        else
          anns
      member.t.asPointer.get.element match {
        case newStruct: TClass[Post] =>
          // We recurse, since a field is another struct
          anns ++ unwrapStructPerm(
            loc,
            newStruct,
            origin,
            makeStruct,
            structType +: visited,
          )
        case _ => anns
      }
    })

    newFieldPerms.flatten
  }

  case class MakeAnns(
      i: Variable[Post],
      j: Variable[Post],
      size: Expr[Post],
      trigger: Expr[Post],
      triggerUnique: Seq[Expr[Post]],
  ) {
    def makePerm(
        location: Variable[Post] => Location[Post],
        blame: Blame[ReceiverNotInjective],
    ): Expr[Post] = {
      implicit val o: Origin = arrayCreationOrigin
      val zero = const[Post](0)
      val body =
        (zero <= i.get && i.get < size) ==> Perm(location(i), WritePerm[Post]())
      Starall(Seq(i), Seq(Seq(trigger)), body)(blame)
    }

    def makeUnique(access: Variable[Post] => Expr[Post]): Expr[Post] = {
      implicit val o: Origin = arrayCreationOrigin
      val zero = const[Post](0)
      val pre1 = zero <= i.get && i.get < size
      val pre2 = zero <= j.get && j.get < size
      val body =
        (pre1 && pre2 && (i.get !== j.get)) ==> (access(i) !== access(j))
      Forall(Seq(i, j), Seq(triggerUnique), body)
    }
  }

  def typeIsRef(t: Type[_]): Boolean =
    t match {
      case _: TClass[_] => true
      case _ => false
    }

  def makePointerCreationMethodFor(
      elementType: Type[Pre],
      nullable: Boolean,
  ) = {
    implicit val o: Origin = arrayCreationOrigin
    // ar != null
    // ar.length == dim0
    // forall ar[i] :: Perm(ar[i], write)
    // (if type ar[i] is pointer or struct):
    // forall i,j :: i!=j ==> ar[i] != ar[j]
    val sizeArg = new Variable[Post](TInt())(o.where(name = "size"))
    val zero = const[Post](0)

    globalDeclarations.declare(withResult((result: Result[Post]) => {
      val requires = sizeArg.get >= zero
      val i = new Variable[Post](TInt())(o.where(name = "i"))
      val j = new Variable[Post](TInt())(o.where(name = "j"))
      val access =
        (i: Variable[Post]) => PointerSubscript(result, i.get)(FramedPtrOffset)

      val makeStruct = MakeAnns(
        i,
        j,
        sizeArg.get,
        access(i),
        Seq(access(i), access(j)),
      )

      var ensures =
        (PointerBlockLength(result)(FramedPtrBlockLength) === sizeArg.get) &*
          (PointerBlockOffset(result)(FramedPtrBlockOffset) === zero)

      if (nullable) { ensures = (result !== Null()) &* ensures }
      // Pointer location needs pointer add, not pointer subscript
      ensures =
        ensures &* makeStruct.makePerm(
          i =>
            PointerLocation(PointerAdd(result, i.get)(FramedPtrOffset))(
              FramedPtrOffset
            ),
          IteratedPtrInjective,
        )
      ensures =
        if (!typeIsRef(elementType))
          ensures
        else { ensures &* makeStruct.makeUnique(access) }

      val permFields =
        dispatch(elementType) match {
          case t: TClass[Post] => unwrapStructPerm(access, t, o, makeStruct)
          case _ => Seq()
        }

      ensures =
        if (permFields.isEmpty)
          ensures
        else
          ensures &* foldStar(permFields.map(_._1))

      procedure(
        blame = AbstractApplicable,
        contractBlame = TrueSatisfiable,
        returnType =
          if (nullable) { TPointer(dispatch(elementType)) }
          else { TNonNullPointer(dispatch(elementType)) },
        args = Seq(sizeArg),
        requires = UnitAccountedPredicate(requires),
        ensures = UnitAccountedPredicate(ensures),
      )(o.where(name = "make_pointer_array_" + elementType.toString))
    }))
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case values @ Values(arr, from, to) =>
        val arrayType = CoercionUtils.getAnyArrayCoercion(arr.t).get._2
        val func = valuesFunctions
          .getOrElseUpdate(arrayType, makeFunctionFor(arrayType))
        FunctionInvocation[Post](
          func.ref,
          Seq(dispatch(arr), dispatch(from), dispatch(to)),
          Nil,
          Nil,
          Nil,
        )(NoContext(ArrayValuesPreconditionFailed(values)))
      case newArr @ NewArray(element, dims, moreDims, initialize) =>
        val method = arrayCreationMethods.getOrElseUpdate(
          (element, dims.size, moreDims, initialize),
          makeCreationMethodFor(element, dims.size, moreDims, initialize),
        )
        ProcedureInvocation[Post](
          method.ref,
          dims.map(dispatch),
          Nil,
          Nil,
          Nil,
          Nil,
        )(ArrayCreationFailed(newArr))
      case newPointerArr @ NewPointerArray(element, size) =>
        val method = pointerArrayCreationMethods.getOrElseUpdate(
          element,
          makePointerCreationMethodFor(element, nullable = true),
        )
        ProcedureInvocation[Post](
          method.ref,
          Seq(dispatch(size)),
          Nil,
          Nil,
          Nil,
          Nil,
        )(PointerArrayCreationFailed(newPointerArr, newPointerArr.blame))
      case newPointerArr @ NewNonNullPointerArray(element, size) =>
        val method = nonNullPointerArrayCreationMethods.getOrElseUpdate(
          element,
          makePointerCreationMethodFor(element, nullable = false),
        )
        ProcedureInvocation[Post](
          method.ref,
          Seq(dispatch(size)),
          Nil,
          Nil,
          Nil,
          Nil,
        )(PointerArrayCreationFailed(newPointerArr, newPointerArr.blame))
      case free @ FreePointer(xs) =>
        val newXs = dispatch(xs)
        val TPointer(t) = newXs.t
        val (freeFunc, freeBlame) = freeMethods.getOrElseUpdate(t, makeFree(t))
        ProcedureInvocation[Post](freeFunc.ref, Seq(newXs), Nil, Nil, Nil, Nil)(
          freeBlame(free)
        )(free.o)
      case other => rewriteDefault(other)
    }
  }
}
