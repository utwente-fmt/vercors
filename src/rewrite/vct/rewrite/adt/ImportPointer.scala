package vct.col.rewrite.adt

import vct.col.ast._
import ImportADT.typeText
import hre.util.ScopedStack
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{ClassToRef, Generation}
import vct.col.util.AstBuildHelpers.{functionInvocation, _}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.Unreachable

import scala.collection.mutable

case object ImportPointer extends ImportADTBuilder("pointer") {
  private def PointerField(t: Type[_], uniqueId: Option[BigInt]): Origin =
    Origin(Seq(
      PreferredName(Seq(typeText(t) + uniqueId.map(_.toString).getOrElse(""))),
      LabelContext("pointer field"),
    ))

  private val PointerCreationOrigin: Origin = Origin(
    Seq(LabelContext("adtPointer, pointer creation method"))
  )

  private val AsTypeOrigin: Origin = Origin(
    Seq(LabelContext("adtPointer, asType function"))
  )

  private val PointerToAdtOrigin: Origin = Origin(
    Seq(LabelContext("adtPointer, pointerToAdt function"))
  )

  private case class PointerNullOptNone(
      inner: Blame[PointerNull],
      expr: Expr[_],
  ) extends Blame[OptionNone] {
    override def blame(error: OptionNone): Unit = inner.blame(PointerNull(expr))
  }

  private case class PointerBoundsPreconditionFailed(
      inner: Blame[PointerBounds],
      expr: Node[_],
  ) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(PointerBounds(expr))
  }

  private case class DerefPointerBoundsPreconditionFailed(
      inner: Blame[PointerDerefError],
      expr: Expr[_],
  ) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      inner.blame(PointerInsufficientPermission(expr))
  }

  private case class PointerFieldInsufficientPermission(
      inner: Blame[PointerInsufficientPermission],
      expr: Expr[_],
  ) extends Blame[InsufficientPermission] {
    override def blame(error: InsufficientPermission): Unit =
      inner.blame(PointerInsufficientPermission(expr))
  }

  private sealed trait Context
  private final case class InAxiom() extends Context
}

case class ImportPointer[Pre <: Generation](importer: ImportADTImporter)
    extends ImportADT[Pre](importer) {
  import ImportPointer._

  private lazy val pointerFile = parse("pointer")

  private lazy val blockAdt = find[AxiomaticDataType[Post]](
    pointerFile,
    "block",
  )
  private lazy val blockBase = find[ADTFunction[Post]](blockAdt, "base_addr")
  private lazy val blockLength = find[ADTFunction[Post]](
    blockAdt,
    "block_length",
  )
  private lazy val blockLoc = find[ADTFunction[Post]](blockAdt, "loc")
  private lazy val pointerAdt = find[AxiomaticDataType[Post]](
    pointerFile,
    "pointer",
  )
  private lazy val pointerOf = find[ADTFunction[Post]](pointerAdt, "pointer_of")
  private lazy val pointerBlock = find[ADTFunction[Post]](
    pointerAdt,
    "pointer_block",
  )
  private lazy val pointerOffset = find[ADTFunction[Post]](
    pointerAdt,
    "pointer_offset",
  )
  private lazy val pointerDeref = find[Function[Post]](pointerFile, "ptr_deref")
  private lazy val pointerAdd = find[Function[Post]](pointerFile, "ptr_add")
  private lazy val pointerAddress = find[ADTFunction[Post]](
    pointerAdt,
    "ptr_address",
  )
  private lazy val pointerFromAddress = find[Function[Post]](
    pointerFile,
    "ptr_from_address",
  )

  private val pointerField
      : mutable.Map[(Type[Post], Option[BigInt]), SilverField[Post]] = mutable
    .Map()

  private val pointerCreationMethods
      : SuccessionMap[TNonNullPointer[Pre], Procedure[Post]] = SuccessionMap()

  private val asTypeFunctions
      : SuccessionMap[(Type[Pre], Type[Pre]), Function[Post]] = SuccessionMap()
  private val toAdtFunctions
      : mutable.Map[(AxiomaticDataType[Pre], Seq[Type[Pre]]), Function[Post]] =
    mutable.Map()
  private val context: ScopedStack[Context] = ScopedStack()
  private var casts: Set[(Type[Pre], Type[Pre], Expr[Pre], Expr[Pre])] =
    Set.empty
  private var inMakeAsType: ScopedStack[Unit] = ScopedStack()

  private def makeAsTypeFunction(
      from: Type[Pre],
      to: Type[Pre],
      fromSize: Expr[Pre],
      toSize: Expr[Pre],
  ): Function[Post] = {
    implicit val o: Origin = AsTypeOrigin
      .where(name = "as_" + to.toString + "_from_" + from.toString)
//        val orig = new Variable[Post](TAxiomatic(pointerAdt.ref, Nil))(o.where(name = "orig"))
//        val inv: Function[Post] = globalDeclarations.declare(
//          function[Post](
//            AbstractApplicable,
//            TrueSatisfiable,
//            returnType = TAxiomatic(pointerAdt.ref, Nil),
//            args = Seq(orig),
//          )(o.where(name = "as_" + toName + "_from_" + fromName + "_inv"))
//        )
    if (inMakeAsType.isEmpty && !asTypeFunctions.contains((to, from))) {
      inMakeAsType.having(()) {
        asTypeFunctions((to, from)) = makeAsTypeFunction(
          to,
          from,
          toSize,
          fromSize,
        )
      }
    }
    val value =
      new Variable[Post](TAxiomatic(pointerAdt.ref, Nil))(
        AsTypeOrigin.where(name = "value")
      )
    globalDeclarations.declare(withResult((result: Result[Post]) =>
      function[Post](
        AbstractApplicable,
        TrueSatisfiable,
//        ensures = UnitAccountedPredicate(foldAnd(casts.map(t =>
//          functionInvocation[Post](
//            TrueSatisfiable,
//            asTypeFunctions.ref(t),
//            Seq(result),
//          ) === value.get
//        ))),
        ensures = UnitAccountedPredicate(
//                  And(
//                  functionInvocation[Post](TrueSatisfiable, inv.ref, args=Seq(result)) === value.get,
          adtFunctionInvocation[Post](
            pointerAddress.ref,
            args = Seq(result, dispatch(toSize)),
          ) === adtFunctionInvocation[Post](
            pointerAddress.ref,
            args = Seq(value.get, dispatch(fromSize)),
          )
//                  )
        ),
        returnType = TAxiomatic(pointerAdt.ref, Nil),
        args = Seq(value),
      )
    ))
  }

  private def makePointerCreationMethod(
      pointerT: TNonNullPointer[Pre],
      newT: Type[Post],
  ): Procedure[Post] = {
    val t = pointerT.element
    implicit val o: Origin = PointerCreationOrigin
      .where(name = "create_nonnull_pointer_" + newT.toString)

    val result =
      new Variable[Post](TAxiomatic(pointerAdt.ref, Nil))(o.where(name = "res"))
    var ensures =
      (ADTFunctionInvocation[Post](
        typeArgs = Some((blockAdt.ref, Nil)),
        ref = blockLength.ref,
        args = Seq(ADTFunctionInvocation[Post](
          typeArgs = Some((pointerAdt.ref, Nil)),
          ref = pointerBlock.ref,
          args = Seq(result.get),
        )),
      ) === const(1)) &*
        (ADTFunctionInvocation[Post](
          typeArgs = Some((pointerAdt.ref, Nil)),
          ref = pointerOffset.ref,
          args = Seq(result.get),
        ) === const(0))
    pointerT.element match {
      // TODO: Using a label to keep track of this information is quite ugly and I should replace it with something better
      case TAxiomatic(adt, _)
          if adt.decl.o.find[LabelContext]
            .exists(_.label == ClassToRef.ByValueClassADTLabel) =>
      case _ =>
        ensures =
          ensures &* Perm(
            SilverFieldLocation(
              obj =
                FunctionInvocation[Post](
                  ref = pointerDeref.ref,
                  args = Seq(result.get),
                  typeArgs = Nil,
                  Nil,
                  Nil,
                )(PanicBlame("ptr_deref requires nothing.")),
              field =
                pointerField.getOrElseUpdate(
                  (newT, pointerT.unique), {
                    globalDeclarations.declare(new SilverField(newT)(
                      PointerField(newT, pointerT.unique)
                    ))
                  },
                ).ref,
            ),
            WritePerm(),
          )
    }
    globalDeclarations.declare(procedure[Post](
      blame = AbstractApplicable,
      contractBlame = TrueSatisfiable,
      returnType = TVoid(),
      outArgs = Seq(result),
      ensures = UnitAccountedPredicate(
        ensures /*&* (asType(t, result.get) === result.get)*/
      ),
      decreases = Some(DecreasesClauseNoRecursion[Post]()),
    ))
  }

  private def getPointerField(ptr: Expr[Pre]): Ref[Post, SilverField[Post]] = {
    val ptrT = ptr.t.asPointer.get
    val tElement = dispatch(ptrT.element)
    pointerField.getOrElseUpdate(
      (tElement, ptrT.unique), {
        globalDeclarations.declare(new SilverField(tElement)(
          PointerField(tElement, ptrT.unique)
        ))
      },
    ).ref
  }

  private def unwrapOption(
      ptr: Expr[Pre],
      blame: Blame[PointerNull],
  ): Expr[Post] = {
    ptr.t match {
      case TPointer(_, _) =>
        dispatch(ptr) match {
          case OptSome(inner) => inner
          case newPtr => OptGet(newPtr)(PointerNullOptNone(blame, ptr))(ptr.o)
        }
      case TNonNullPointer(_, _) => dispatch(ptr)
    }
  }

  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(
      implicit o: Origin
  ): Expr[Post] =
    coercion match {
      case CoerceNullPointer(_) => OptNoneTyped(TAxiomatic(pointerAdt.ref, Nil))
      case CoerceNonNullPointer(_) => OptSome(e)
      case other => super.applyCoercion(e, other)
    }

  override def postCoerce(program: Program[Pre]): Program[Post] = {
    casts =
      program.collect { case PointerCast(from, to, fromSize, toSize) =>
        (
          from.t.asPointer.get.element,
          to.asPointer.get.element,
          fromSize,
          toSize,
        )
      }.toSet
    super.postCoerce(program)
  }

  override def postCoerce(decl: Declaration[Pre]): Unit = {
    decl match {
      case axiom: ADTAxiom[Pre] =>
        context.having(InAxiom()) {
          allScopes.anySucceed(axiom, axiom.rewriteDefault())
        }
//      // TODO: This is an ugly way to exempt this one bit of generated code from having ptrAdd's added
//      case proc: Procedure[Pre]
//          if proc.o.find[LabelContext]
//            .exists(_.label == "classToRef cast helpers") =>
//        context.having(InAxiom()) {
//          allScopes.anySucceed(proc, proc.rewriteDefault())
//        }
      case adt: AxiomaticDataType[Pre]
          if adt.o.find[SourceName].exists(_.name == "pointer") =>
        implicit val o: Origin = adt.o
        context.having(InAxiom()) {
          globalDeclarations.succeed(
            adt,
            adt.rewrite(decls = {
              val adtSucc = succ[AxiomaticDataType[Post]](adt)
              val addrSucc = succ[ADTFunction[Post]](adt.decls.collectFirst {
                case f: ADTFunction[Pre]
                    if f.o.find[SourceName].exists(_.name == "ptr_address") =>
                  f
              }.get)
              aDTDeclarations.collect {
                adt.decls.foreach(dispatch)
                casts.map { case (from1, to1, _, _) =>
                  aDTDeclarations.declare(new ADTAxiom[Post](forall(
                    TAxiomatic(adtSucc, Nil),
                    body = { p =>
                      functionInvocation[Post](
                        TrueSatisfiable,
                        asTypeFunctions.ref((to1, from1)),
                        Seq(functionInvocation[Post](
                          TrueSatisfiable,
                          asTypeFunctions.ref((from1, to1)),
                          Seq(p),
                        )),
                      ) === p
                    },
                    triggers = { p =>
                      Seq(Seq(functionInvocation[Post](
                        TrueSatisfiable,
                        asTypeFunctions.ref((to1, from1)),
                        Seq(functionInvocation[Post](
                          TrueSatisfiable,
                          asTypeFunctions.ref((from1, to1)),
                          Seq(p),
                        )),
                      )))
                    },
                  )))
                }

                aDTDeclarations.declare(new ADTAxiom[Post](foralls(
                  Seq(TAxiomatic(adtSucc, Nil), TInt()),
                  body = { case Seq(p, stride) =>
                    // TODO: Stop hardcoding this number!
                    LessEq(
                      adtFunctionInvocation(addrSucc, args = Seq(p, stride)),
                      const(BigInt("18446744073709551615")),
                    )
                  },
                  triggers = { case Seq(p, stride) =>
                    Seq(Seq(
                      adtFunctionInvocation(addrSucc, args = Seq(p, stride))
                    ))
                  },
                )))
              }._1
            }),
          )
        }
      case _ => super.postCoerce(decl)
    }
  }

  override def postCoerce(t: Type[Pre]): Type[Post] =
    t match {
      case TPointer(_, _) => TOption(TAxiomatic(pointerAdt.ref, Nil))
      case TNonNullPointer(_, _) => TAxiomatic(pointerAdt.ref, Nil)
      case other => super.postCoerce(other)
    }

  override def postCoerce(location: Location[Pre]): Location[Post] = {
    implicit val o: Origin = location.o
    location match {
      case loc @ PointerLocation(pointer) =>
        val arg =
          unwrapOption(pointer, loc.blame) match {
            case inv @ FunctionInvocation(ref, _, _, _, _, _)
                if ref.decl == pointerAdd.ref.decl =>
              inv
            case ptr =>
              FunctionInvocation[Post](
                ref = pointerAdd.ref,
                args = Seq(ptr, const(0)),
                typeArgs = Nil,
                Nil,
                Nil,
              )(PanicBlame("ptrAdd(ptr, 0) should be infallible"))
          }
        SilverFieldLocation(
          obj =
            FunctionInvocation[Post](
              ref = pointerDeref.ref,
              args = Seq(arg),
              typeArgs = Nil,
              Nil,
              Nil,
            )(PanicBlame("ptr_deref requires nothing."))(pointer.o),
          field = getPointerField(pointer),
        )
      case other => other.rewriteDefault()
    }
  }

  override def postCoerce(s: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = s.o
    s match {
      case scope: Scope[Pre] =>
        scope.rewrite(body = Block(scope.locals.collect {
          case v if v.t.isInstanceOf[TNonNullPointer[Pre]] => {
            val firstUse = scope.body.collectFirst {
              case l @ Local(Ref(variable)) if variable == v => l
            }
            if (
              firstUse.isDefined && scope.body.collectFirst {
                case Assign(l @ Local(Ref(variable)), _) if variable == v =>
                  System.identityHashCode(l) !=
                    System.identityHashCode(firstUse.get)
              }.getOrElse(true)
            ) {
              val oldT = v.t.asInstanceOf[TNonNullPointer[Pre]]
              val newT = dispatch(oldT.element)
              Seq(
                InvokeProcedure[Post](
                  pointerCreationMethods.getOrElseUpdate(
                    oldT,
                    makePointerCreationMethod(oldT, newT),
                  ).ref,
                  Nil,
                  Seq(Local(succ(v))),
                  Nil,
                  Nil,
                  Nil,
                )(TrueSatisfiable)
              )
            } else { Nil }
          }
        }.flatten :+ dispatch(scope.body)))
      case _ => s.rewriteDefault()
    }
  }

  private def rewriteTopLevelPointerSubscriptInTrigger(
      e: Expr[Pre]
  ): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case add @ PointerAdd(pointer, offset) =>
        FunctionInvocation[Post](
          ref = pointerAdd.ref,
          args = Seq(unwrapOption(pointer, add.blame), dispatch(offset)),
          typeArgs = Nil,
          Nil,
          Nil,
        )(NoContext(PointerBoundsPreconditionFailed(add.blame, pointer)))
      case sub @ PointerSubscript(pointer, index) =>
        FunctionInvocation[Post](
          ref = pointerDeref.ref,
          args = Seq(
            FunctionInvocation[Post](
              ref = pointerAdd.ref,
              args = Seq(unwrapOption(pointer, sub.blame), dispatch(index)),
              typeArgs = Nil,
              Nil,
              Nil,
            )(NoContext(PointerBoundsPreconditionFailed(sub.blame, index)))
          ),
          typeArgs = Nil,
          Nil,
          Nil,
        )(PanicBlame("ptr_deref requires nothing."))
      case deref @ DerefPointer(pointer) =>
        FunctionInvocation[Post](
          ref = pointerDeref.ref,
          args = Seq(if (!context.topOption.contains(InAxiom())) {
            FunctionInvocation[Post](
              ref = pointerAdd.ref,
              // Always index with zero, otherwise quantifiers with pointers do not get triggered
              args = Seq(unwrapOption(pointer, deref.blame), const(0)),
              typeArgs = Nil,
              Nil,
              Nil,
            )(NoContext(
              DerefPointerBoundsPreconditionFailed(deref.blame, pointer)
            ))
          } else { unwrapOption(pointer, deref.blame) }),
          typeArgs = Nil,
          Nil,
          Nil,
        )(PanicBlame("ptr_deref requires nothing."))
      case other => dispatch(other)
    }
  }

  override def postCoerce(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case f @ Forall(_, triggers, _)
          if !f.o.find[LabelContext]
            .exists(_.label == "generated quantifier") =>
        f.rewrite(triggers =
          triggers.map(_.map(rewriteTopLevelPointerSubscriptInTrigger))
        )
      case s @ Starall(_, triggers, _) =>
        s.rewrite(triggers =
          triggers.map(_.map(rewriteTopLevelPointerSubscriptInTrigger))
        )
      case e @ Exists(_, triggers, _) =>
        e.rewrite(triggers =
          triggers.map(_.map(rewriteTopLevelPointerSubscriptInTrigger))
        )
      case sub @ PointerSubscript(pointer, index) =>
        SilverDeref(
          obj =
            FunctionInvocation[Post](
              ref = pointerDeref.ref,
              args = Seq(
                FunctionInvocation[Post](
                  ref = pointerAdd.ref,
                  args = Seq(unwrapOption(pointer, sub.blame), dispatch(index)),
                  typeArgs = Nil,
                  Nil,
                  Nil,
                )(NoContext(PointerBoundsPreconditionFailed(sub.blame, index)))
              ),
              typeArgs = Nil,
              Nil,
              Nil,
            )(PanicBlame("ptr_deref requires nothing.")),
          field = getPointerField(pointer),
        )(PointerFieldInsufficientPermission(sub.blame, sub))
      case add @ PointerAdd(pointer, offset) =>
        val inv =
          FunctionInvocation[Post](
            ref = pointerAdd.ref,
            args = Seq(unwrapOption(pointer, add.blame), dispatch(offset)),
            typeArgs = Nil,
            Nil,
            Nil,
          )(NoContext(PointerBoundsPreconditionFailed(add.blame, pointer)))
        pointer.t match {
          case TPointer(_, _) => OptSome(inv)
          case TNonNullPointer(_, _) => inv
        }
      case deref @ DerefPointer(pointer) =>
        SilverDeref(
          obj =
            FunctionInvocation[Post](
              ref = pointerDeref.ref,
              args = Seq(if (!context.topOption.contains(InAxiom())) {
                FunctionInvocation[Post](
                  ref = pointerAdd.ref,
                  // Always index with zero, otherwise quantifiers with pointers do not get triggered
                  args = Seq(unwrapOption(pointer, deref.blame), const(0)),
                  typeArgs = Nil,
                  Nil,
                  Nil,
                )(NoContext(
                  DerefPointerBoundsPreconditionFailed(deref.blame, pointer)
                ))
              } else { unwrapOption(pointer, deref.blame) }),
              typeArgs = Nil,
              Nil,
              Nil,
            )(PanicBlame("ptr_deref requires nothing.")),
          field = getPointerField(pointer),
        )(PointerFieldInsufficientPermission(deref.blame, deref))
      case len @ PointerBlockLength(pointer) =>
        ADTFunctionInvocation[Post](
          typeArgs = Some((blockAdt.ref, Nil)),
          ref = blockLength.ref,
          args = Seq(ADTFunctionInvocation[Post](
            typeArgs = Some((pointerAdt.ref, Nil)),
            ref = pointerBlock.ref,
            args = Seq(unwrapOption(pointer, len.blame)),
          )),
        )
      case off @ PointerBlockOffset(pointer) =>
        ADTFunctionInvocation[Post](
          typeArgs = Some((pointerAdt.ref, Nil)),
          ref = pointerOffset.ref,
          args = Seq(unwrapOption(pointer, off.blame)),
        )
      case pointerLen @ PointerLength(pointer) =>
        postCoerce(
          PointerBlockLength(pointer)(pointerLen.blame) -
            PointerBlockOffset(pointer)(pointerLen.blame)
        )
      case PointerCast(value, targetType, fromSize, toSize) =>
        val newValue = dispatch(value)
        (targetType, value.t) match {
          case (target: PointerType[Pre], value: PointerType[Pre])
              if target.unique != value.unique &&
                target.element == value.element =>
            // Should not occur
            ???
          case (TPointer(innerType, _), TPointer(original, _)) =>
            Select[Post](
              OptEmpty(newValue),
              OptNoneTyped(TAxiomatic(pointerAdt.ref, Nil)),
              OptSome(applyAsTypeFunction(
                original,
                innerType,
                fromSize,
                toSize,
                value,
                OptGet(newValue)(PanicBlame(
                  "Can never be null since this is ensured in the conditional expression"
                )),
              )),
            )
          case (TNonNullPointer(innerType, _), TPointer(original, _)) =>
            applyAsTypeFunction(
              original,
              innerType,
              fromSize,
              toSize,
              value,
              OptGet(newValue)(PanicBlame(
                "Casting a pointer to a non-null pointer implies the pointer must be statically known to be non-null"
              )),
            )
          case (TPointer(innerType, _), TNonNullPointer(original, _)) =>
            OptSome(applyAsTypeFunction(
              original,
              innerType,
              fromSize,
              toSize,
              value,
              newValue,
            ))
          case (TNonNullPointer(innerType, _), TNonNullPointer(original, _)) =>
            applyAsTypeFunction(
              original,
              innerType,
              fromSize,
              toSize,
              value,
              newValue,
            )
          case (_, _) =>
            throw Unreachable(
              s"Pointer cast with unknown pointer types: $targetType and ${value.t}"
            )
        }
      case IntegerPointerCast(value, targetType, typeSize) =>
        val newValue = dispatch(value)
        (targetType, value.t) match {
          case (TInt() | TBoundedInt(_, _), TPointer(_, None)) =>
            letIfNonTrivial(
              dispatch(value.t),
              newValue,
              { v =>
                Select[Post](
                  OptEmpty(v),
                  const(0),
                  adtFunctionInvocation[Post](
                    ref = pointerAddress.ref,
                    args = Seq(
                      OptGet(v)(PanicBlame(
                        "Can never be null since this is ensured in the conditional expression"
                      )),
                      dispatch(typeSize),
                    ),
                  ),
                )
              },
            )
          case (TInt() | TBoundedInt(_, _), TNonNullPointer(_, _)) =>
            adtFunctionInvocation[Post](
              ref = pointerAddress.ref,
              args = Seq(newValue, dispatch(typeSize)),
            )
          case (TPointer(_, None), TInt() | TBoundedInt(_, _)) =>
            letIfNonTrivial(
              dispatch(value.t),
              newValue,
              { v =>
                Select[Post](
                  v === const(0),
                  OptNoneTyped(TAxiomatic(pointerAdt.ref, Nil)),
                  OptSome(
                    FunctionInvocation[Post](
                      ref = pointerFromAddress.ref,
                      args = Seq(v, dispatch(typeSize)),
                      typeArgs = Nil,
                      Nil,
                      Nil,
                    )(PanicBlame("Stride > 0"))
                  ),
                )
              },
            )
          case (TNonNullPointer(_, None), TInt() | TBoundedInt(_, _)) =>
            FunctionInvocation[Post](
              ref = pointerFromAddress.ref,
              args = Seq(newValue, dispatch(typeSize)),
              typeArgs = Nil,
              Nil,
              Nil,
            )(PanicBlame("Stride > 0")) // TODO: Blame??
        }
      case blck @ PointerBlock(p) =>
        ADTFunctionInvocation[Post](
          typeArgs = None,
          ref = pointerBlock.ref,
          args = Seq(unwrapOption(p, blck.blame)),
        )
      case addr @ PointerAddress(p, elementSize) =>
        adtFunctionInvocation[Post](
          ref = pointerAddress.ref,
          args = Seq(unwrapOption(p, addr.blame), dispatch(elementSize)),
        )
      case to @ PointerToAdt(p, TAxiomatic(Ref(adt), args)) =>
        functionInvocation(
          TrueSatisfiable,
          ref =
            toAdtFunctions.getOrElseUpdate(
              (adt, args), {
                globalDeclarations.declare(
                  function[Post](
                    AbstractApplicable,
                    TrueSatisfiable,
                    TAxiomatic(succ(adt), args.map(dispatch)),
                    Seq(new Variable[Post](TAxiomatic(pointerAdt.ref, Nil))(
                      PointerToAdtOrigin.where(name = "p")
                    )),
                  )(PointerToAdtOrigin.where(name =
                    "ptr_to_" + adt.o.find[SourceName].map(_.name)
                      .getOrElse("unknown")
                  ))
                )
              },
            ).ref,
          args = Seq(p match {
            case PointerAdd(_, _) => unwrapOption(p, to.blame)
            case _ if context.topOption.contains(InAxiom()) =>
              unwrapOption(p, to.blame)
            case _ =>
              FunctionInvocation[Post](
                ref = pointerAdd.ref,
                args = Seq(unwrapOption(p, to.blame), const(0)),
                typeArgs = Nil,
                Nil,
                Nil,
              )(PanicBlame(
                "Pointer out of bounds, but this should not be possible since index equals 0"
              ))
          }),
        )
      case other => super.postCoerce(other)
    }
  }

  private def applyAsTypeFunction(
      fromType: Type[Pre],
      toType: Type[Pre],
      fromSize: Expr[Pre],
      toSize: Expr[Pre],
      preExpr: Expr[Pre],
      postExpr: Expr[Post],
  )(implicit o: Origin): Expr[Post] = {
    asType(
      fromType,
      toType,
      fromSize,
      toSize,
      preExpr match {
        case PointerAdd(_, _) => postExpr
        // Don't add ptrAdd in an ADT axiom since we cannot use functions with preconditions there
        case _ if context.topOption.contains(InAxiom()) => postExpr
        case _ =>
          FunctionInvocation[Post](
            ref = pointerAdd.ref,
            // Always index with zero, otherwise quantifiers with pointers do not get triggered
            args = Seq(postExpr, const(0)),
            typeArgs = Nil,
            Nil,
            Nil,
          )(PanicBlame(
            "Pointer out of bounds in pointer cast (no appropriate blame available)"
          ))
      },
    )
  }

  private def asType(
      fromType: Type[Pre],
      toType: Type[Pre],
      fromSize: Expr[Pre],
      toSize: Expr[Pre],
      expr: Expr[Post],
  )(implicit o: Origin): Expr[Post] = {
    functionInvocation[Post](
      PanicBlame("as_type requires nothing"),
      asTypeFunctions.getOrElseUpdate(
        (fromType, toType),
        makeAsTypeFunction(fromType, toType, fromSize, toSize),
      ).ref,
      Seq(expr),
    )
  }
}
