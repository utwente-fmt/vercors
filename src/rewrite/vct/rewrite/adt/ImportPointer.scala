package vct.col.rewrite.adt

import vct.col.ast._
import ImportADT.typeText
import hre.util.ScopedStack
import vct.col.origin._
import vct.col.ref.{LazyRef, Ref}
import vct.col.rewrite.{ClassToRef, Generation, RewriterBuilderArg3}
import vct.col.util.AstBuildHelpers.{functionInvocation, _}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.{Unreachable, UserError}

import scala.collection.mutable

case object ImportPointer
    extends RewriterBuilderArg3[ImportADTImporter, String, String] {
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

  case class DerefAddToSubscriptBlame(
      dpBlame: Blame[PointerDerefError],
      addBlame: Blame[PointerAddError],
  ) extends Blame[PointerSubscriptError] {
    override def blame(error: PointerSubscriptError): Unit =
      error match {
        case error: PointerDerefError => dpBlame.blame(error)
        case bounds: PointerBounds => addBlame.blame(bounds)
      }
  }
  private case class UnknownEncoding(encoding: String) extends UserError {
    override def code: String = "unknownEncoding"

    override def text: String =
      s"Offset encoding `$encoding` is not known, expected: `default`, `fixed`, or `sequenced`"
  }

  private sealed trait Context
  private final case class InAxiom() extends Context

  override def key: String = "adtPointer"

  override def desc: String =
    s"Import types into vercors that are defined externally, usually via an axiomatic datatype. This pass imports pointer."
}

case class ImportPointer[Pre <: Generation](
    importer: ImportADTImporter,
    arrayEncoding: String,
    offsetEncoding: String,
) extends ImportADT[Pre](importer) {
  import ImportPointer._

  private lazy val pointerFile =
    offsetEncoding match {
      case "default" => parse("pointer")
      case "fixed" => parse("pointer_fixed")
      case _ => throw UnknownEncoding(offsetEncoding)
    }

  private lazy val blockAdt = find[AxiomaticDataType[Post]](
    pointerFile,
    "block",
  )
  private lazy val blockAddress = find[ADTFunction[Post]](
    blockAdt,
    "block_address",
  )
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
  private lazy val pointerLoc = find[ADTFunction[Post]](pointerAdt, "loc")
  private lazy val pointerAdd = find[Function[Post]](pointerFile, "ptr_add")
  private lazy val pointerAddress = find[ADTFunction[Post]](
    pointerAdt,
    "ptr_address",
  )
  private lazy val pointerCastHelperAdt = find[AxiomaticDataType[Post]](
    pointerFile,
    "PointerCastHelper",
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
  private var casts: Set[Type[Pre]] = Set.empty
  private val inMakeAsType: ScopedStack[Unit] = ScopedStack()
  private val fromCastHelperFunctions
      : SuccessionMap[Type[Pre], Function[Post]] = SuccessionMap()
  private val toCastHelperFunctions: SuccessionMap[Type[Pre], Function[Post]] =
    SuccessionMap()

  private def makeFromCastHelperFunction(t: Type[Pre]): Function[Post] = {
    implicit val o: Origin = AsTypeOrigin
      .where(name = "from_cast_helper_" + t.toString)
    val value =
      new Variable[Post](TAxiomatic(new LazyRef(pointerCastHelperAdt), Nil))(
        AsTypeOrigin.where(name = "helper")
      )
    globalDeclarations.declare(function[Post](
      AbstractApplicable,
      TrueSatisfiable,
      returnType = TAxiomatic(new LazyRef(pointerAdt), Nil),
      args = Seq(value),
    ))
  }

  private def makeToCastHelperFunction(t: Type[Pre]): Function[Post] = {
    implicit val o: Origin = AsTypeOrigin
      .where(name = "to_cast_helper_" + t.toString)
    val value =
      new Variable[Post](TAxiomatic(new LazyRef(pointerAdt), Nil))(
        AsTypeOrigin.where(name = "ptr")
      )
    globalDeclarations.declare(withResult((result: Result[Post]) =>
      function[Post](
        AbstractApplicable,
        TrueSatisfiable,
        returnType = TAxiomatic(new LazyRef(pointerCastHelperAdt), Nil),
        args = Seq(value),
      )
    ))
  }

  private def makeAsTypeFunction(
      from: Type[Pre],
      to: Type[Pre],
      fromSize: Expr[Pre],
      toSize: Expr[Pre],
  ): Function[Post] = {
    implicit val o: Origin = AsTypeOrigin
      .where(name = "as_" + to.toString + "_from_" + from.toString)
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
        ensures = UnitAccountedPredicate(
          result === functionInvocation[Post](
            TrueSatisfiable,
            fromCastHelperFunctions
              .getOrElseUpdate(to, makeFromCastHelperFunction(to)).ref,
            Seq(functionInvocation[Post](
              TrueSatisfiable,
              toCastHelperFunctions
                .getOrElseUpdate(from, makeToCastHelperFunction(from)).ref,
              Seq(value.get),
            )),
          ) &&
            getAddress(result, dispatch(toSize)) ===
            getAddress(value.get, dispatch(fromSize))
        ),
        returnType = TAxiomatic(pointerAdt.ref, Nil),
        args = Seq(value),
      )
    ))
  }

  private def getAddress(p: Expr[Post], size: Expr[Post])(
      implicit o: Origin
  ): Expr[Post] =
    offsetEncoding match {
      case "default" =>
        adtFunctionInvocation[Post](pointerAddress.ref, args = Seq(p, size))
      case "fixed" =>
        adtFunctionInvocation[Post](pointerAddress.ref, args = Seq(p))
      case _ => throw UnknownEncoding(offsetEncoding)
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
        (offsetEncoding match {
          case "default" =>
            ADTFunctionInvocation[Post](
              typeArgs = Some((pointerAdt.ref, Nil)),
              ref = pointerOffset.ref,
              args = Seq(result.get),
            ) === const(0)
          case "fixed" =>
            adtFunctionInvocation[Post](
              pointerAddress.ref,
              args = Seq(result.get),
            ) === adtFunctionInvocation[Post](
              blockAddress.ref,
              args = Seq(
                adtFunctionInvocation(pointerBlock.ref, args = Seq(result.get))
              ),
            )
          case _ => throw UnknownEncoding(offsetEncoding)
        })
    pointerT.element match {
      // TODO: Using a label to keep track of this information is quite ugly and I should replace it with something better
      case TAxiomatic(adt, _)
          if adt.decl.o.find[LabelContext]
            .exists(_.label == ClassToRef.ByValueClassADTLabel) =>
      case _ =>
        ensures =
          ensures &* Perm(
            SilverFieldLocation(
              obj = derefPointer(result.get),
              field =
                pointerField.getOrElseUpdate(
                  (newT, pointerT.unique), {
                    globalDeclarations.declare(new SilverField(newT)(
                      PointerField(t, pointerT.unique)
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
          PointerField(ptrT.element, ptrT.unique)
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
      program.flatCollect {
        case PointerCast(from, to, _, _) if from.t != to =>
          Seq(from.t.asPointer.get.element, to.asPointer.get.element)
      }.toSet
    super.postCoerce(program)
  }

  override def postCoerce(decl: Declaration[Pre]): Unit = {
    decl match {
      case axiom: ADTAxiom[Pre] =>
        context.having(InAxiom()) {
          allScopes.anySucceed(axiom, axiom.rewriteDefault())
        }
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
                casts.map { t =>
                  aDTDeclarations.declare(new ADTAxiom[Post](forall(
                    TAxiomatic(new LazyRef(pointerCastHelperAdt), Nil),
                    body = { p =>
                      InlinePattern(functionInvocation[Post](
                        TrueSatisfiable,
                        toCastHelperFunctions
                          .getOrElseUpdate(t, makeToCastHelperFunction(t)).ref,
                        Seq(functionInvocation[Post](
                          TrueSatisfiable,
                          fromCastHelperFunctions
                            .getOrElseUpdate(t, makeFromCastHelperFunction(t))
                            .ref,
                          Seq(p),
                        )),
                      )) === p
                    },
                  )))
                  aDTDeclarations.declare(new ADTAxiom[Post](forall(
                    TAxiomatic(adtSucc, Nil),
                    body = { p =>
                      InlinePattern(functionInvocation[Post](
                        TrueSatisfiable,
                        fromCastHelperFunctions
                          .getOrElseUpdate(t, makeFromCastHelperFunction(t))
                          .ref,
                        Seq(functionInvocation[Post](
                          TrueSatisfiable,
                          toCastHelperFunctions
                            .getOrElseUpdate(t, makeToCastHelperFunction(t))
                            .ref,
                          Seq(p),
                        )),
                      )) === p
                    },
                  )))
                }

                if (arrayEncoding == "sequenced" || arrayEncoding == "nested") {
                  val blockSucc = succ[ADTFunction[Post]](
                    adt.decls.collectFirst {
                      case f: ADTFunction[Pre]
                          if f.o.find[SourceName]
                            .exists(_.name == "pointer_block") =>
                        f
                    }.get
                  )
                  val offsetSucc = succ[ADTFunction[Post]](
                    adt.decls.collectFirst {
                      case f: ADTFunction[Pre]
                          if f.o.find[SourceName]
                            .exists(_.name == "pointer_offset") =>
                        f
                    }.get
                  )
                  aDTDeclarations.declare(new ADTAxiom[Post](foralls(
                    Seq(TAxiomatic(adtSucc, Nil), TAxiomatic(adtSucc, Nil)),
                    body = { case Seq(p1, p2) =>
                      (InlinePattern(
                        adtFunctionInvocation(blockSucc, args = Seq(p1))
                      ) === InlinePattern(
                        adtFunctionInvocation(blockSucc, args = Seq(p2))
                      ) && InlinePattern(
                        adtFunctionInvocation(offsetSucc, args = Seq(p1))
                      ) === InlinePattern(
                        adtFunctionInvocation(offsetSucc, args = Seq(p2))
                      )) ==> (p1 === p2)
                    },
                  )))
                }

                aDTDeclarations.declare(offsetEncoding match {
                  case "default" =>
                    new ADTAxiom[Post](foralls(
                      Seq(TAxiomatic(adtSucc, Nil), TInt()),
                      body = { case Seq(p, stride) =>
                        // TODO: Stop hardcoding this number!
                        LessEq(
                          adtFunctionInvocation(
                            addrSucc,
                            args = Seq(p, stride),
                          ),
                          const(BigInt("18446744073709551615")),
                        )
                      },
                      triggers = { case Seq(p, stride) =>
                        Seq(Seq(
                          adtFunctionInvocation(addrSucc, args = Seq(p, stride))
                        ))
                      },
                    ))
                  case "fixed" =>
                    new ADTAxiom[Post](forall(
                      TAxiomatic(adtSucc, Nil),
                      body = { p =>
                        LessEq(
                          adtFunctionInvocation(addrSucc, args = Seq(p)),
                          const(BigInt("18446744073709551615")),
                        )
                      },
                    ))
                  case _ => throw UnknownEncoding(offsetEncoding)
                })
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

  private def ptrAdd(p: Expr[Post], offset: Expr[Post], stride: Expr[Post])(
      blame: Blame[InvocationFailure]
  )(implicit o: Origin): Expr[Post] =
    offsetEncoding match {
      case "default" =>
        functionInvocation(blame, pointerAdd.ref, args = Seq(p, offset))
      case "fixed" =>
        functionInvocation(blame, pointerAdd.ref, args = Seq(p, offset, stride))
      case _ => throw UnknownEncoding(offsetEncoding)
    }

  override def postCoerce(location: Location[Pre]): Location[Post] = {
    implicit val o: Origin = location.o
    location match {
      case loc @ PointerLocation(pointer, typeSize) =>
        val arg =
          unwrapOption(pointer, loc.blame) match {
            case inv @ FunctionInvocation(ref, _, _, _, _, _)
                if ref.decl == pointerAdd.ref.decl =>
              inv
            case ptr =>
              ptrAdd(ptr, const(0), dispatch(typeSize))(PanicBlame(
                "ptrAdd(ptr, 0) should be infallible"
              ))
          }
        SilverFieldLocation(
          obj = derefPointer(arg)(pointer.o),
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
      case ApplyCoercion(expr, coercion) =>
        applyCoercion(rewriteTopLevelPointerSubscriptInTrigger(expr), coercion)
      case off @ PointerBlockOffset(pointer, _) =>
        offsetEncoding match {
          case "default" =>
            ADTFunctionInvocation[Post](
              typeArgs = Some((pointerAdt.ref, Nil)),
              ref = pointerOffset.ref,
              args = Seq(unwrapOption(pointer, off.blame)),
            )
          case "fixed" =>
            adtFunctionInvocation[Post](
              pointerAddress.ref,
              args = Seq(unwrapOption(pointer, off.blame)),
            )
          case _ => throw UnknownEncoding(offsetEncoding)
        }
      case len @ PointerBlockLength(pointer, size) =>
        ADTFunctionInvocation[Post](
          typeArgs = Some((blockAdt.ref, Nil)),
          ref = blockLength.ref,
          args = Seq(ADTFunctionInvocation[Post](
            typeArgs = Some((pointerAdt.ref, Nil)),
            ref = pointerBlock.ref,
            args = Seq(unwrapOption(pointer, len.blame)),
          )),
        )
      case add @ PointerAdd(pointer, offset, size) =>
        ptrAdd(
          unwrapOption(pointer, add.blame),
          dispatch(offset),
          dispatch(size),
        )(NoContext(PointerBoundsPreconditionFailed(add.blame, pointer)))
      case sub @ PointerSubscript(pointer, index, size) =>
        derefPointer(
          ptrAdd(
            unwrapOption(pointer, sub.blame),
            dispatch(index),
            dispatch(size),
          )(NoContext(PointerBoundsPreconditionFailed(sub.blame, pointer)))
        )
      case DerefPointer(add @ PointerAdd(pointer, offset, size), _) =>
        derefPointer(
          ptrAdd(
            unwrapOption(pointer, add.blame),
            dispatch(offset),
            dispatch(size),
          )(NoContext(PointerBoundsPreconditionFailed(add.blame, pointer)))
        )
      case deref @ DerefPointer(pointer, typeSize) =>
        derefPointer(if (!context.topOption.contains(InAxiom())) {
          ptrAdd(
            unwrapOption(pointer, deref.blame),
            const(0),
            dispatch(typeSize),
          )(NoContext(
            DerefPointerBoundsPreconditionFailed(deref.blame, pointer)
          ))
        } else { unwrapOption(pointer, deref.blame) })
      case other => dispatch(other)
    }
  }

  override def preCoerce(e: Expr[Pre]): Expr[Pre] = {
    implicit val o: Origin = e.o
    e match {
      case d @ DerefPointer(a @ PointerAdd(p, i, size), _) =>
        PointerSubscript(p, i, size)(DerefAddToSubscriptBlame(d.blame, a.blame))
      case _ => super.preCoerce(e)
    }
  }

  override def postCoerce(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case f @ Forall(_, triggers, _)
          /*if !f.o.find[LabelContext]
            .exists(_.label == "generated quantifier")*/ =>
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
      case sub @ PointerSubscript(pointer, index, size) =>
        SilverDeref(
          obj = derefPointer(
            ptrAdd(
              unwrapOption(pointer, sub.blame),
              dispatch(index),
              dispatch(size),
            )(NoContext(PointerBoundsPreconditionFailed(sub.blame, pointer)))
          ),
          field = getPointerField(pointer),
        )(PointerFieldInsufficientPermission(sub.blame, sub))
      case add @ PointerAdd(pointer, offset, size) =>
        val inv =
          ptrAdd(
            unwrapOption(pointer, add.blame),
            dispatch(offset),
            dispatch(size),
          )(NoContext(PointerBoundsPreconditionFailed(add.blame, pointer)))
        pointer.t match {
          case TPointer(_, _) => OptSome(inv)
          case TNonNullPointer(_, _) => inv
        }
      case deref @ DerefPointer(pointer, typeSize) =>
        SilverDeref(
          obj = derefPointer(if (!context.topOption.contains(InAxiom())) {
            ptrAdd(
              unwrapOption(pointer, deref.blame),
              const(0),
              dispatch(typeSize),
            )(NoContext(
              DerefPointerBoundsPreconditionFailed(deref.blame, pointer)
            ))
          } else { unwrapOption(pointer, deref.blame) }),
          field = getPointerField(pointer),
        )(PointerFieldInsufficientPermission(deref.blame, deref))
      case InlinePattern(
            len @ PointerBlockLength(pointer, size),
            parent,
            group,
          ) =>
        val length = InlinePattern(
          ADTFunctionInvocation[Post](
            typeArgs = Some((blockAdt.ref, Nil)),
            ref = blockLength.ref,
            args = Seq(ADTFunctionInvocation[Post](
              typeArgs = Some((pointerAdt.ref, Nil)),
              ref = pointerBlock.ref,
              args = Seq(unwrapOption(pointer, len.blame)),
            )),
          ),
          parent,
          group,
        )
        offsetEncoding match {
          case "default" => length
          case "fixed" => length / dispatch(size)
          case _ => throw UnknownEncoding(offsetEncoding)
        }
      case len @ PointerBlockLength(pointer, size) =>
        val length = ADTFunctionInvocation[Post](
          typeArgs = Some((blockAdt.ref, Nil)),
          ref = blockLength.ref,
          args = Seq(ADTFunctionInvocation[Post](
            typeArgs = Some((pointerAdt.ref, Nil)),
            ref = pointerBlock.ref,
            args = Seq(unwrapOption(pointer, len.blame)),
          )),
        )
        offsetEncoding match {
          case "default" => length
          case "fixed" => length / dispatch(size)
          case _ => throw UnknownEncoding(offsetEncoding)
        }

      case InlinePattern(
            off @ PointerBlockOffset(pointer, size),
            parent,
            group,
          ) =>
        offsetEncoding match {
          case "default" =>
            InlinePattern(
              ADTFunctionInvocation[Post](
                typeArgs = Some((pointerAdt.ref, Nil)),
                ref = pointerOffset.ref,
                args = Seq(unwrapOption(pointer, off.blame)),
              ),
              parent,
              group,
            )
          case "fixed" =>
            InlinePattern(
              adtFunctionInvocation[Post](
                pointerAddress.ref,
                args = Seq(unwrapOption(pointer, off.blame)),
              ),
              parent,
              group,
            ) - adtFunctionInvocation[Post](
              blockAddress.ref,
              args = Seq(postCoerce(PointerBlock(pointer)(off.blame))),
            ) / dispatch(size)
          case _ => throw UnknownEncoding(offsetEncoding)
        }
      case off @ PointerBlockOffset(pointer, size) =>
        offsetEncoding match {
          case "default" =>
            ADTFunctionInvocation[Post](
              typeArgs = Some((pointerAdt.ref, Nil)),
              ref = pointerOffset.ref,
              args = Seq(unwrapOption(pointer, off.blame)),
            )
          case "fixed" =>
            (adtFunctionInvocation[Post](
              pointerAddress.ref,
              args = Seq(unwrapOption(pointer, off.blame)),
            ) - adtFunctionInvocation[Post](
              blockAddress.ref,
              args = Seq(postCoerce(PointerBlock(pointer)(off.blame))),
            )) / dispatch(size)
          case _ => throw UnknownEncoding(offsetEncoding)
        }
      case pointerLen @ PointerLength(pointer, size) =>
        postCoerce(
          PointerBlockLength(pointer, size)(pointerLen.blame) -
            PointerBlockOffset(pointer, size)(pointerLen.blame)
        )
      case to @ ToNonNull(value) =>
        OptGet(dispatch(value))(PointerNullOptNone(to.blame, value))
      case PointerCast(value, targetType, fromSize, toSize) =>
        val newValue = dispatch(value)
        (targetType, value.t) match {
          case (a, b) if a == b => newValue
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
      case blck @ PointerBlock(p) =>
        ADTFunctionInvocation[Post](
          typeArgs = None,
          ref = pointerBlock.ref,
          args = Seq(unwrapOption(p, blck.blame)),
        )
      case addr @ PointerAddress(p, elementSize) =>
        getAddress(unwrapOption(p, addr.blame), dispatch(elementSize))
      case to @ PointerToAdt(p, TAxiomatic(Ref(adt), args), typeSize) =>
        functionInvocation(
          TrueSatisfiable,
          ref =
            toAdtFunctions.getOrElseUpdate(
              (adt, args), {
                val name =
                  "ptr_to_" + adt.o.find[SourceName].map(_.name)
                    .getOrElse("unknown")
                val inv = globalDeclarations.declare(
                  function[Post](
                    AbstractApplicable,
                    TrueSatisfiable,
                    TAxiomatic(pointerAdt.ref, Nil),
                    Seq(
                      new Variable[Post](
                        TAxiomatic(succ(adt), args.map(dispatch))
                      )(PointerToAdtOrigin.where(name = "a"))
                    ),
                  )(PointerToAdtOrigin.where(name = name + "_inv"))
                )
                val p =
                  new Variable[Post](TAxiomatic(pointerAdt.ref, Nil))(
                    PointerToAdtOrigin.where(name = "p")
                  )
                globalDeclarations.declare(withResult((result: Result[Post]) =>
                  function[Post](
                    AbstractApplicable,
                    TrueSatisfiable,
                    TAxiomatic(succ(adt), args.map(dispatch)),
                    Seq(p),
                    ensures = UnitAccountedPredicate(
                      p.get === functionInvocation(
                        TrueSatisfiable,
                        ref = inv.ref,
                        args = Seq(result),
                      )
                    ),
                  )(PointerToAdtOrigin.where(name = name))
                ))
              },
            ).ref,
          args = Seq(p match {
            case ApplyCoercion(PointerAdd(_, _, _), CoerceIdentity(_)) |
                PointerAdd(_, _, _) =>
              unwrapOption(p, to.blame)
            case _ if context.topOption.contains(InAxiom()) =>
              unwrapOption(p, to.blame)
            case _ =>
              ptrAdd(unwrapOption(p, to.blame), const(0), dispatch(typeSize))(
                PanicBlame(
                  "Pointer out of bounds, but this should not be possible since index equals 0"
                )
              )
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
        case ApplyCoercion(PointerAdd(_, _, _), CoerceIdentity(_)) |
            PointerAdd(_, _, _) =>
          postExpr
        // Don't add ptrAdd in an ADT axiom since we cannot use functions with preconditions there
        case _ if context.topOption.contains(InAxiom()) => postExpr
        case _ =>
          ptrAdd(postExpr, const(0), dispatch(toSize))(PanicBlame(
            "Pointer out of bounds, but this should not be possible since index equals 0"
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

  private def derefPointer(p: Expr[Post])(implicit o: Origin): Expr[Post] =
    offsetEncoding match {
      case "default" =>
        FunctionInvocation[Post](
          ref = pointerDeref.ref,
          args = Seq(p),
          typeArgs = Nil,
          Nil,
          Nil,
        )(PanicBlame("ptr_deref requires nothing."))
      case "fixed" => adtFunctionInvocation(pointerLoc.ref, args = Seq(p))
      case _ => throw UnknownEncoding(offsetEncoding)
    }
}
