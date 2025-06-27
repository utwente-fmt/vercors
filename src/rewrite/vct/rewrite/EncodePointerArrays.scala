package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast.{
  ADTAxiom,
  ADTFunction,
  AccountedPredicate,
  AddrOf,
  AnyFunctionInvocation,
  AnyMethodInvocation,
  ApplyCoercion,
  AssignExpression,
  AssignStmt,
  Assuming,
  AxiomaticDataType,
  ByValueClassLocation,
  CoerceConstPointerArrayPointer,
  CoerceConstPointerPointerArray,
  CoercePointerArrayPointer,
  CoercePointerPointerArray,
  Coercion,
  ConstructorInvocation,
  ContractApplicable,
  Declaration,
  DerefPointer,
  Expr,
  FramedProof,
  FunctionInvocation,
  InlinePattern,
  InstanceFunctionInvocation,
  IntegerPointerCast,
  Invocation,
  InvocationStatement,
  InvokeConstructor,
  InvokeMethod,
  InvokeProcedure,
  InvokingNode,
  IterationContract,
  LLVMLoopContract,
  Local,
  Loop,
  LoopInvariant,
  MethodInvocation,
  Mult,
  NewConstPointerArray,
  NewPointerArray,
  Node,
  Null,
  ParBarrier,
  ParBlock,
  ParParallel,
  ParRegion,
  ParSequential,
  Perm,
  PointerAdd,
  PointerArraySubscript,
  PointerArrayType,
  PointerBlockLength,
  PointerBlockOffset,
  PointerCast,
  PointerLength,
  PointerLocation,
  PointerSubscript,
  Predicate,
  Procedure,
  ProcedureInvocation,
  Program,
  Result,
  Scope,
  ScopedExpr,
  SplitAccountedPredicate,
  Statement,
  TAxiomatic,
  TConstPointer,
  TConstPointerArray,
  TInt,
  TNonNullConstPointer,
  TNonNullPointer,
  TPointer,
  TPointerArray,
  ToNonNull,
  Type,
  UnitAccountedPredicate,
  Variable,
  WritePerm,
}
import vct.col.origin.{
  AbstractApplicable,
  Blame,
  FramedPtrOffset,
  InstanceInvocationFailure,
  InvocationFailure,
  IteratedPtrInjective,
  LabelContext,
  MismatchedArrayDimension,
  NodeVerificationFailure,
  NonNullCoercionError,
  NonNullPointerNull,
  Origin,
  PanicBlame,
  PointerAddError,
  PointerBounds,
  PointerNull,
  PointerSubscriptError,
  PreBlameSplit,
  PreconditionFailed,
  TrueSatisfiable,
  UnsafeCoercion,
}
import vct.col.ref.Ref
import vct.col.rewrite.EncodeArrayValues.PointerArrayCreationFailed
import vct.col.rewrite.error.ExtraNode
import vct.col.rewrite.{Generation, RewriterBuilder}
import vct.col.typerules.CoercingRewriter
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.{Unreachable, UserError}

import scala.collection.mutable

case object EncodePointerArrays extends RewriterBuilder {
  override def key: String = "encodePointerArray"

  override def desc: String = "Encodes (multi-dimensional) C-style arrays"

  private case class InvalidPatternLocation(node: Node[_]) extends UserError {
    override def code: String = "pointerArrayTriggerPattern"

    override def text: String =
      node.o
        .messageInContext("We do not support putting a trigger pattern here")
  }

  private case class CalculatedPointerAddBlame(
      blame: Blame[PointerSubscriptError]
  ) extends Blame[PointerAddError] {
    override def blame(error: PointerAddError): Unit =
      error match {
        case PointerNull(_) =>
          PanicBlame("It should not be possible to get a nullable PointerArray")
            .blame(error)
        case bounds @ PointerBounds(_) => blame.blame(bounds)
      }
  }

  private case class MismatchedArrayDimensionBlame(
      invokingNode: InvokingNode[_],
      dimensionExpr: Expr[_],
      v: Variable[_],
      blame: Blame[InvocationFailure],
  ) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      blame.blame(MismatchedArrayDimension(invokingNode, dimensionExpr, v))
  }

  private case class NonNullCoercionBlame(
      blame: Blame[UnsafeCoercion],
      node: Node[_],
  ) extends Blame[PointerNull] {
    override def blame(error: PointerNull): Unit =
      blame.blame(NonNullCoercionError(node))
  }

  private val ConstructorOrigin: Origin = Origin(
    Seq(LabelContext("Pointer array constructors"))
  )
}

case class EncodePointerArrays[Pre <: Generation]()
    extends CoercingRewriter[Pre] {
  import EncodePointerArrays._

  private val constructors
      : mutable.HashMap[(Type[Pre], Int, Option[BigInt]), Procedure[Post]] =
    mutable.HashMap()
  private val arraySucc: SuccessionMap[
    (Type[Pre], Int, Option[BigInt], Boolean),
    AxiomaticDataType[Post],
  ] = SuccessionMap()
  private val pointerSucc
      : SuccessionMap[(Type[Pre], Int, Option[BigInt], Boolean), ADTFunction[
        Post
      ]] = SuccessionMap()
  private val fromPointerSucc
      : SuccessionMap[(Type[Pre], Int, Option[BigInt], Boolean), ADTFunction[
        Post
      ]] = SuccessionMap()
  private val dimSucc: SuccessionMap[
    (Type[Pre], Int, Option[BigInt], Int, Boolean),
    ADTFunction[Post],
  ] = SuccessionMap()

  private val currentVariableContext: mutable.HashSet[Variable[Pre]] = mutable
    .HashSet()

  private val globalBlame: ScopedStack[Blame[UnsafeCoercion]] = ScopedStack()

  override def postCoerce(program: Program[Pre]): Program[Post] = {
    globalBlame.having(program.blame) {
      program.rewrite(declarations =
        globalDeclarations.dispatch(program.declarations)
      )
    }
  }
  // NOTE 1: We currently do not handle expressions that introduce new variables (Binders, ScopedExpr) of the PointerArray type, the size will not be available in these expressions
  // NOTE 2: This rewriter is a bit aggressive with adding its dimensions requirements everywhere (it basically replicates PropagateContextEverywhere) even if this would be unnecessary. I don't believe this'll significantly hurt performance though

  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(
      implicit o: Origin
  ): Expr[Post] =
    coercion match {
      case CoercePointerArrayPointer(element, dimensions, unique) =>
        initialiseAdt(element, dimensions, unique, isConst = false)
        // Should be safe to check type on Post since it's always a pointer or pointer array type
        e.t match {
          // Only possibility is the new AxiomaticDataType we introduce in this rewriter
          case TAxiomatic(_, _) =>
            adtFunctionInvocation[Post](
              pointerSucc.ref((element, dimensions, unique, false)),
              args = Seq(e),
            )
          case _ => e
        }
      case CoerceConstPointerArrayPointer(element, dimensions) =>
        initialiseAdt(element, dimensions, None, isConst = true)
        // Should be safe to check type on Post since it's always a pointer or pointer array type
        e.t match {
          // Only possibility is the new AxiomaticDataType we introduce in this rewriter
          case TAxiomatic(_, _) =>
            adtFunctionInvocation[Post](
              pointerSucc.ref((element, dimensions, None, true)),
              args = Seq(e),
            )
          case _ => e
        }
      case CoercePointerPointerArray(element, dimensions, unique) =>
        initialiseAdt(element, dimensions.length, unique, isConst = false)
        val newE =
          e.t match {
            case TPointer(_, _) =>
              ToNonNull[Post](e)(NonNullCoercionBlame(globalBlame.top, e))
            case _ => e
          }
        adtFunctionInvocation[Post](
          fromPointerSucc.ref((element, dimensions.length, unique, false)),
          args = Seq(newE),
        )
      case CoerceConstPointerPointerArray(element, dimensions) =>
        initialiseAdt(element, dimensions.length, None, isConst = true)
        val newE =
          e.t match {
            case TConstPointer(_) =>
              ToNonNull[Post](e)(NonNullCoercionBlame(globalBlame.top, e))
            case _ => e
          }
        adtFunctionInvocation[Post](
          fromPointerSucc.ref((element, dimensions.length, None, true)),
          args = Seq(newE),
        )
      case other => super.applyCoercion(e, other)
    }

  override def postCoerce(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o;

    e match {
      case AddrOf(sub @ PointerArraySubscript(a, _)) => calculatePointer(sub)
      case AddrOf(ApplyCoercion(sub @ PointerArraySubscript(a, _), _)) =>
        calculatePointer(sub)
      case AddrOf(ApplyCoercion(inner, c)) =>
        applyCoercion(dispatch(inner), c) match {
          case PointerArraySubscript(_, _) =>
            throw Unreachable(
              "Unexpected non-identity coercions to PointerArraySubscript, missing case in EncodePointerArrays"
            )
          case _ => super.postCoerce(e)
        }
      case AddrOf(inner) if inner.t.asPointerArray.isDefined =>
        val t = inner.t.asPointerArray.get
        adtFunctionInvocation(
          pointerSucc
            .ref((t.element, t.dimensions.length, t.unique, t.isConst)),
          args = Seq(dispatch(inner)),
        )
      case sub @ PointerArraySubscript(a, _)
          if a.t.asPointerArray.get.dimensions.length == 1 =>
        DerefPointer(calculatePointer(sub))(sub.blame)
      case sub @ PointerArraySubscript(_, _) => calculatePointer(sub)
      case npa @ NewPointerArray(element, dimensions, unique) =>
        procedureInvocation(
          PointerArrayCreationFailed(npa, npa.blame),
          initialiseAdt(element, dimensions.length, unique, isConst = false),
          dimensions.map(dispatch),
        )
      case npa @ NewConstPointerArray(element, dimensions) =>
        procedureInvocation(
          PointerArrayCreationFailed(npa, npa.blame),
          initialiseAdt(element, dimensions.length, None, isConst = true),
          dimensions.map(dispatch),
        )
      case IntegerPointerCast(value, t, size)
          if value.t.asPointerArray.isDefined =>
        val arrayT = value.t.asPointerArray.get
        IntegerPointerCast(
          adtFunctionInvocation(
            pointerSucc.ref((
              arrayT.element,
              arrayT.dimensions.length,
              arrayT.unique,
              arrayT.isConst,
            )),
            args = Seq(dispatch(value)),
          ),
          dispatch(t),
          dispatch(size),
        )

      // Support for integer pointer cast here only makes sense if we support pointers to arrays
      // Currently we cannot even parse a cast like this, e.g. (int (*)[8][5])addr
      // case IntegerPointerCast(value, t: PointerType[Pre], size) if t.element.asPointerArray.isDefined =>
      //   val arrayT = t.element
      //   adtFunctionInvocation[Post](fromPointerSucc.ref((arrayT.element, arrayT.dimensions.length, arrayT.unique, arrayT.isConst)), args= Seq(IntegerPointerCast(dispatch(value), t match {
      //     case TPointerArray(element, _, unique) => dispatch(TNonNullPointer(element, unique))
      //     case TConstPointerArray(element, _) => dispatch(TNonNullConstPointer(element))
      //   }, dispatch(size))))
      case inv: Invocation[Pre] =>
        inv match {
          case inv: AnyMethodInvocation[Pre] =>
            inv match {
              case inv: ProcedureInvocation[Pre] =>
                inv.rewrite(blame =
                  rewriteBlame(inv, inv.args, inv.ref.decl.args, inv.blame)
                )
              case inv: MethodInvocation[Pre] =>
                inv.rewrite(blame =
                  rewriteBlame(inv, inv.args, inv.ref.decl.args, inv.blame)
                )
              case inv: ConstructorInvocation[Pre] =>
                inv.rewrite(blame =
                  rewriteBlame(inv, inv.args, inv.ref.decl.args, inv.blame)
                )
            }
          case inv: AnyFunctionInvocation[Pre] =>
            inv match {
              case inv: FunctionInvocation[Pre] =>
                inv.rewrite(blame =
                  rewriteBlame(inv, inv.args, inv.ref.decl.args, inv.blame)
                )
              case inv: InstanceFunctionInvocation[Pre] =>
                inv.rewrite(blame =
                  rewriteBlame(inv, inv.args, inv.ref.decl.args, inv.blame)
                )
            }
        }
      case e: AssignExpression[Pre] =>
        e.target match {
          case Local(Ref(v)) => currentVariableContext += v
          case _ =>
        }
        e.rewriteDefault()

      case _ => super.postCoerce(e)
    }
  }

  private def rewriteBlame[T >: InvocationFailure <: InstanceInvocationFailure](
      invokingNode: InvokingNode[Pre],
      args: Seq[Expr[Pre]],
      declArgs: Seq[Variable[Pre]],
      inBlame: Blame[T],
  ): Blame[T] =
    args.zipWithIndex.flatMap { case (v, i) => v.t.asPointerArray.map((_, i)) }
      .flatMap { case (t, i) =>
        initialiseAdt(t.element, t.dimensions.length, t.unique, t.isConst)
        t.dimensions.filter(_.isDefined).map(d =>
          MismatchedArrayDimensionBlame(
            invokingNode,
            d.get,
            declArgs(i),
            inBlame,
          )
        )
      }.foldLeft(inBlame) { case (r, l) => PreBlameSplit.left(l, r) }

  override def postCoerce(s: Statement[Pre]): Statement[Post] = {
    implicit val o: Origin = s.o
    s match {
      case Scope(variables, _) =>
        // Not adding variables until they're assigned since VerCors scopes don't match normal programming scopes since you can refer to a variable before its declaration
        val res = s.rewriteDefault()
        currentVariableContext --= variables
        res
      case s: AssignStmt[Pre] =>
        s.target match {
          case Local(Ref(v)) => currentVariableContext += v
          case _ =>
        }
        s.rewriteDefault()
      case inv: InvocationStatement[Pre] =>
        inv match {
          case inv: InvokeProcedure[Pre] =>
            inv.rewrite(blame =
              rewriteBlame(inv, inv.args, inv.ref.decl.args, inv.blame)
            )
          case inv: InvokeConstructor[Pre] =>
            inv.rewrite(blame =
              rewriteBlame(inv, inv.args, inv.ref.decl.args, inv.blame)
            )
          case inv: InvokeMethod[Pre] =>
            inv.rewrite(blame =
              rewriteBlame(inv, inv.args, inv.ref.decl.args, inv.blame)
            )
        }
      case loop: Loop[Pre] =>
        loop.contract match {
          case inv @ LoopInvariant(invariant, _) =>
            loop.rewrite(contract =
              inv.rewrite(invariant =
                getDimensionExpr &* super.dispatch(invariant)
              )
            )
          case _: IterationContract[Pre] => throw ExtraNode
          case _: LLVMLoopContract[Pre] => throw ExtraNode
        }
      case bar: ParBarrier[Pre] =>
        bar.rewrite(
          requires = getDimensionExpr &* dispatch(bar.requires),
          ensures = getDimensionExpr &* dispatch(bar.ensures),
        )
      case frame: FramedProof[Pre] =>
        frame.rewrite(
          pre = getDimensionExpr &* dispatch(frame.pre),
          post = getDimensionExpr &* dispatch(frame.post),
        )
      case _ => super.postCoerce(s)
    }
  }

  override def postCoerce(parRegion: ParRegion[Pre]): ParRegion[Post] = {
    implicit val o: Origin = parRegion.o

    parRegion match {
      case block: ParBlock[Pre] =>
        block.rewrite(
          requires = getDimensionExpr &* dispatch(block.requires),
          ensures = getDimensionExpr &* dispatch(block.ensures),
        )
      case _: ParParallel[Pre] | _: ParSequential[Pre] =>
        parRegion.rewriteDefault()
    }
  }

  private def getDimensionExpr(implicit o: Origin): Expr[Post] = {
    foldAnd(
      currentVariableContext.flatMap(v => v.t.asPointerArray.map((v, _)))
        .flatMap { case (v, t) =>
          implicit val o: Origin = v.o.where(context = "Dimension invariant")
          val dimensions = t.dimensions.length
          initialiseAdt(t.element, dimensions, t.unique, t.isConst)
          t.dimensions.zipWithIndex.filter { case (d, _) => d.isDefined }
            .map { case (d, i) =>
              adtFunctionInvocation[Post](
                dimSucc.ref((t.element, dimensions, t.unique, i, t.isConst)),
                args = Seq(Local(succ(v))),
              ) === dispatch(d.get)
            }
        }
    )
  }

  override def postCoerce(t: Type[Pre]): Type[Post] =
    t match {
      case a: PointerArrayType[Pre] =>
        TAxiomatic(
          arraySucc.ref((a.element, a.dimensions.length, a.unique, a.isConst)),
          Nil,
        )
      case _ => super.postCoerce(t)
    }

  override def postCoerce(decl: Declaration[Pre]): Unit = {
    implicit val o: Origin = decl.o
    decl match {
      case app: ContractApplicable[Pre] =>
        val requires =
          (oldRequires: AccountedPredicate[Post]) =>
            app.args.flatMap { v => v.t.asPointerArray.map((v, _)) }.flatMap {
              case (v, t) =>
                val dimensions = t.dimensions.length
                initialiseAdt(t.element, dimensions, t.unique, t.isConst)
                t.dimensions.zipWithIndex.filter { case (d, _) => d.isDefined }
                  .map { case (d, i) =>
                    UnitAccountedPredicate(
                      adtFunctionInvocation[Post](
                        dimSucc
                          .ref((t.element, dimensions, t.unique, i, t.isConst)),
                        args = Seq(Local(succ(v))),
                      ) === dispatch(d.get)
                    )
                  }
            }.foldLeft(oldRequires) { case (r, l) =>
              SplitAccountedPredicate(l, r)
            }
        currentVariableContext ++= app.args
        currentVariableContext ++= app.contract.givenArgs
        currentVariableContext ++= app.contract.yieldsArgs
        allScopes.anySucceed(
          app,
          app.rewrite(contract =
            app.contract.rewrite(requires =
              requires(app.contract.requires.rewriteDefault())
            )
          ),
        )
        currentVariableContext --= app.args
        currentVariableContext --= app.contract.givenArgs
        currentVariableContext --= app.contract.yieldsArgs
      case p: Predicate[Pre] if p.body.isDefined =>
        currentVariableContext ++= p.args
        globalDeclarations.succeed(
          p,
          p.rewrite(body = Some(getDimensionExpr &* dispatch(p.body.get))),
        )
        currentVariableContext --= p.args
      case _ => super.postCoerce(decl)
    }
  }

  private def calculatePointer(sub: PointerArraySubscript[Pre]): Expr[Post] = {
    implicit val o: Origin = sub.o
    val arrayT = sub.array.t.asPointerArray.get
    val (obj, index, length) = calculateOffset(sub, arrayT.dimensions.length)
    initialiseAdt(arrayT.element, length, arrayT.unique, arrayT.isConst)
    PointerAdd(
      adtFunctionInvocation[Post](
        pointerSucc
          .ref((arrayT.element, length, arrayT.unique, arrayT.isConst)),
        args = Seq(obj),
      ),
      index,
    )(CalculatedPointerAddBlame(sub.blame))
  }

  private def calculateOffset(
      sub: PointerArraySubscript[Pre],
      depth: Int,
  ): (Expr[Post], Expr[Post], Int) = {
    implicit val o: Origin = sub.o
    val arrayT = sub.array.t.asPointerArray.get
    val (obj, index, length) =
      sub.array match {
        case p: InlinePattern[Pre] => throw InvalidPatternLocation(p)
        case inner: PointerArraySubscript[Pre] =>
          calculateOffset(inner, depth + 1)
        case other =>
          (super.dispatch(other), const[Post](0), arrayT.dimensions.length)
      }
    val newIndex = {
      Seq.range(length - depth + 1, length).map(i =>
        adtFunctionInvocation[Post](
          dimSucc.ref(arrayT.element, length, arrayT.unique, i, arrayT.isConst),
          args = Seq(obj),
        )
      ).fold(super.dispatch(sub.index))(Mult(_, _)) + index
    }
    (obj, newIndex, length)
  }

  private def initialiseAdt(
      element: Type[Pre],
      length: Int,
      unique: Option[BigInt],
      isConst: Boolean,
  ): Ref[Post, Procedure[Post]] = {
    constructors.getOrElseUpdate(
      (element, length, unique),
      createConstructor(element, length, unique, isConst),
    ).ref
  }

  private def createConstructor(
      element: Type[Pre],
      dimensions: Int,
      unique: Option[BigInt],
      isConst: Boolean,
  ): Procedure[Post] = {
    implicit val o: Origin = ConstructorOrigin
    val axiomType = TAxiomatic[Post](
      arraySucc.ref((element, dimensions, unique, isConst)),
      Nil,
    )
    val dimFunctions = Seq.range(0, dimensions).map { i =>
      val f =
        new ADTFunction(
          Seq(new Variable[Post](axiomType)(o.where(name = "array"))),
          TInt(),
        )(o.where(name = s"get_dim_${i}_$element"))
      dimSucc((element, dimensions, unique, i, isConst)) = f
      f
    }
    val pointerType =
      if (isConst)
        TNonNullConstPointer(dispatch(element))
      else { TNonNullPointer(dispatch(element), unique) }
    val pointerFunction =
      new ADTFunction(
        Seq(new Variable[Post](axiomType)(o.where(name = "array"))),
        pointerType,
      )(o.where(name = s"get_${element}_pointer"))
    pointerSucc((element, dimensions, unique, isConst)) = pointerFunction
    val invFunction =
      new ADTFunction(
        Seq(new Variable[Post](pointerType)(o.where(name = "ptr"))),
        axiomType,
      )(o.where(name = s"from_${element}_pointer"))
    fromPointerSucc((element, dimensions, unique, isConst)) = invFunction
    val invAxiom =
      new ADTAxiom[Post](forall(
        pointerType,
        { term =>
          adtFunctionInvocation[Post](
            pointerFunction.ref,
            args = Seq(InlinePattern(
              adtFunctionInvocation(invFunction.ref, args = Seq(term))
            )),
          ) === term
        },
      ))
    val ptr =
      (t: Expr[Post]) =>
        adtFunctionInvocation[Post](pointerFunction.ref, args = Seq(t))
    val boundsAxiom =
      new ADTAxiom[Post](forall(
        axiomType,
        { term =>
          InlinePattern(
            PointerBlockLength(ptr(term))(NonNullPointerNull),
            group = 1,
          ) - InlinePattern(
            PointerBlockOffset(ptr(term))(NonNullPointerNull),
            group = 2,
          ) ===
            dimFunctions
              .map(f => adtFunctionInvocation[Post](f.ref, args = Seq(term)))
              .reduce((a: Expr[Post], b: Expr[Post]) => Mult(a, b))
        },
      ))
    arraySucc((element, dimensions, unique, isConst)) = globalDeclarations
      .declare(
        new AxiomaticDataType(
          dimFunctions ++
            Seq(pointerFunction, invFunction, invAxiom, boundsAxiom),
          Nil,
        )(o.where(name =
          if (isConst) { s"const_pointer_${dimensions}_array_$element" }
          else { s"pointer_${dimensions}_array_$element" }
        ))
      )
    val args = Seq.range(0, dimensions)
      .map(i => new Variable[Post](TInt())(o.where(name = s"dim_$i")))
    globalDeclarations.declare(withResult((result: Result[Post]) => {
      procedure(
        AbstractApplicable,
        TrueSatisfiable,
        axiomType,
        args = args,
        requires = UnitAccountedPredicate(foldAnd(args.map(_.get > const(0)))),
        ensures = {
          val max = args.map(_.get).reduce[Expr[Post]] { (a, b) => a * b }
          val range =
            (term: Local[Post]) => const[Post](0) <= term && term < max
          val trigger =
            (term: Local[Post]) =>
              PointerSubscript(ptr(result), term)(FramedPtrOffset)
          val bounds =
            foldAnd(dimFunctions.zip(args).map { case (f, a) =>
              adtFunctionInvocation[Post](f.ref, args = Seq(result)) === a.get
            }) && PointerBlockLength(ptr(result))(NonNullPointerNull) === max &&
              PointerBlockOffset(ptr(result))(NonNullPointerNull) === const(0)
          val l =
            if (isConst)
              bounds
            else {
              bounds &* starall(
                IteratedPtrInjective,
                TInt(),
                body = { term =>
                  range(term) ==> Perm(
                    PointerLocation(
                      PointerAdd(ptr(result), term)(FramedPtrOffset)
                    )(NonNullPointerNull),
                    WritePerm(),
                  )
                },
                triggers = t => Seq(Seq(trigger(t))),
              )
            }
          UnitAccountedPredicate(if (element.asByValueClass.isDefined) {
            l &* starall(
              IteratedPtrInjective,
              TInt(),
              body = { term =>
                range(term) ==> Perm(
                  ByValueClassLocation(
                    PointerSubscript(ptr(result), term)(FramedPtrOffset)
                  ),
                  WritePerm(),
                )
              },
              triggers = t => Seq(Seq(trigger(t))),
            )
          } else { l })
        },
      )(o.where(name =
        if (isConst) { s"create_const_pointer_${dimensions}_array_$element" }
        else { s"create_pointer_${dimensions}_array_$element" }
      ))
    }))
  }
}
