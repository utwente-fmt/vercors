package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast.util.ExpressionEqualityCheck
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
  AxiomaticDataType,
  ByValueClassLocation,
  CoerceConstPointerArrayPointer,
  CoerceNonNullPointerArray,
  CoerceNullPointerArray,
  CoercePointerArrayPointer,
  CoercePointerNonNullPointerArray,
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
  Label,
  LabelDecl,
  Local,
  Loop,
  LoopInvariant,
  MethodInvocation,
  Mult,
  NewConstPointerArray,
  NewPointerArray,
  Node,
  Null,
  Old,
  OptEmpty,
  OptGet,
  OptNoneTyped,
  OptSome,
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
  PointerEq,
  PointerLocation,
  PointerNeq,
  PointerSubscript,
  PointerType,
  Predicate,
  Procedure,
  ProcedureInvocation,
  Program,
  Result,
  Scope,
  Select,
  SplitAccountedPredicate,
  Statement,
  TAxiomatic,
  TInt,
  TNonNullConstPointer,
  TNonNullPointer,
  TOption,
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
  NonNullCoercionError,
  NonNullPointerNull,
  OptionNone,
  Origin,
  PanicBlame,
  PointerAddError,
  PointerArraySubscriptError,
  PointerBounds,
  PointerNull,
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
      blame: Blame[PointerArraySubscriptError]
  ) extends Blame[PointerAddError] {
    override def blame(error: PointerAddError): Unit =
      error match {
        case PointerNull(_) =>
          PanicBlame("The pointer inside the pointer array ADT cannot be null")
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

  private case class PointerNullOptNone(
      inner: Blame[PointerNull],
      expr: Expr[_],
  ) extends Blame[OptionNone] {
    override def blame(error: OptionNone): Unit = inner.blame(PointerNull(expr))
  }

  private val ConstructorOrigin: Origin = Origin(
    Seq(LabelContext("Pointer array constructors"))
  )
}

case class EncodePointerArrays[Pre <: Generation]()
    extends CoercingRewriter[Pre] {
  import EncodePointerArrays._

  private val constructors
      : mutable.HashMap[(Type[Pre], Int, Option[BigInt], Boolean), Procedure[
        Post
      ]] = mutable.HashMap()
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

  private val variableHeapLabel: SuccessionMap[Variable[Pre], LabelDecl[Post]] =
    SuccessionMap()

  private val currentStatementLabel
      : ScopedStack[mutable.ArrayBuffer[LabelDecl[Post]]] = ScopedStack()

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

  private def unwrapOption(
      ptr: Expr[Pre],
      blame: Blame[PointerNull],
  ): Expr[Post] = {
    ptr.t match {
      case t: PointerArrayType[Pre] if t.isNonNull => dispatch(ptr)
      case t: PointerArrayType[Pre] =>
        dispatch(ptr) match {
          case OptSome(inner) => inner
          case newPtr => OptGet(newPtr)(PointerNullOptNone(blame, ptr))(ptr.o)
        }
    }
  }

  override def applyCoercion(e: => Expr[Post], coercion: Coercion[Pre])(
      implicit o: Origin
  ): Expr[Post] =
    coercion match {
      case CoerceNullPointerArray(target) =>
        val t = target.asPointerArray.get
        initialiseAdt(t.element, t.dimensions.length, t.unique, t.isConst)
        OptNoneTyped(TAxiomatic(
          arraySucc.ref((t.element, t.dimensions.length, t.unique, t.isConst)),
          Nil,
        ))
      case CoercePointerArrayPointer(element, dimensions, unique) =>
        initialiseAdt(element, dimensions, unique, isConst = false)
        // Should be safe to check type on Post since it's always a pointer or pointer array type
        e.t match {
          // Only possibility is the new AxiomaticDataType we introduce in this rewriter
          case TOption(TAxiomatic(_, _)) =>
            Select(
              OptEmpty(e),
              Null(),
              adtFunctionInvocation[Post](
                pointerSucc.ref((element, dimensions, unique, false)),
                args = Seq(OptGet(e)(PanicBlame(
                  "Can never be null since this is ensured in the conditional expression"
                ))),
              ),
            )
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
          case TOption(TAxiomatic(_, _)) =>
            Select(
              OptEmpty(e),
              Null(),
              adtFunctionInvocation[Post](
                pointerSucc.ref((element, dimensions, None, true)),
                args = Seq(OptGet(e)(PanicBlame(
                  "Can never be null since this is ensured in the conditional expression"
                ))),
              ),
            )
          case TAxiomatic(_, _) =>
            adtFunctionInvocation[Post](
              pointerSucc.ref((element, dimensions, None, true)),
              args = Seq(e),
            )
          case _ => e
        }
      case CoercePointerPointerArray(element, dimensions, unique) =>
        initialiseAdt(element, dimensions.length, unique, isConst = false)
        e.t match {
          case t: PointerType[Post] =>
            if (t.isNonNull)
              adtFunctionInvocation[Post](
                fromPointerSucc
                  .ref((element, dimensions.length, unique, false)),
                args = Seq(e),
              )
            else
              Select(
                PointerEq(e, Null(), const(0)),
                OptNoneTyped(TAxiomatic(
                  arraySucc.ref(element, dimensions.length, unique, t.isConst),
                  Nil,
                )),
                OptSome(adtFunctionInvocation[Post](
                  fromPointerSucc
                    .ref((element, dimensions.length, unique, false)),
                  args = Seq(ToNonNull(e)(PanicBlame(
                    "Can never be null since this is ensured in the conditional expression"
                  ))),
                )),
              )
          case _ => e
        }
      case CoercePointerNonNullPointerArray(element, dimensions, unique) =>
        initialiseAdt(element, dimensions.length, unique, isConst = false)
        e.t match {
          case t: PointerType[Post] =>
            if (t.isNonNull) {
              adtFunctionInvocation[Post](
                fromPointerSucc
                  .ref((element, dimensions.length, unique, t.isConst)),
                args = Seq(e),
              )
            } else {
              adtFunctionInvocation[Post](
                fromPointerSucc
                  .ref((element, dimensions.length, unique, t.isConst)),
                args = Seq(
                  ToNonNull(e)(NonNullCoercionBlame(globalBlame.top, e))
                ),
              )
            }
          case _ => e
        }
      case CoerceNonNullPointerArray(_) => OptSome(e)
      case other => super.applyCoercion(e, other)
    }

  override def postCoerce(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o;

    e match {
      case PointerEq(Null() | ApplyCoercion(Null(), _), p, _)
          if p.t.asPointerArray.isDefined =>
        if (p.t.asPointerArray.get.isNonNull)
          ff
        else
          OptEmpty(dispatch(p))
      case PointerEq(Null() | ApplyCoercion(Null(), _), ApplyCoercion(p, _), _)
          if p.t.asPointerArray.isDefined =>
        if (p.t.asPointerArray.get.isNonNull)
          ff
        else
          OptEmpty(dispatch(p))
      case PointerEq(p, Null() | ApplyCoercion(Null(), _), _)
          if p.t.asPointerArray.isDefined =>
        if (p.t.asPointerArray.get.isNonNull)
          ff
        else
          OptEmpty(dispatch(p))
      case PointerEq(ApplyCoercion(p, _), Null() | ApplyCoercion(Null(), _), _)
          if p.t.asPointerArray.isDefined =>
        if (p.t.asPointerArray.get.isNonNull)
          ff
        else
          OptEmpty(dispatch(p))
      case PointerNeq(Null() | ApplyCoercion(Null(), _), p, _)
          if p.t.asPointerArray.isDefined =>
        if (p.t.asPointerArray.get.isNonNull)
          tt
        else
          !OptEmpty(dispatch(p))
      case PointerNeq(Null() | ApplyCoercion(Null(), _), ApplyCoercion(p, _), _)
          if p.t.asPointerArray.isDefined =>
        if (p.t.asPointerArray.get.isNonNull)
          tt
        else
          !OptEmpty(dispatch(p))
      case PointerNeq(p, Null() | ApplyCoercion(Null(), _), _)
          if p.t.asPointerArray.isDefined =>
        if (p.t.asPointerArray.get.isNonNull)
          tt
        else
          !OptEmpty(dispatch(p))
      case PointerNeq(ApplyCoercion(p, _), Null() | ApplyCoercion(Null(), _), _)
          if p.t.asPointerArray.isDefined =>
        if (p.t.asPointerArray.get.isNonNull)
          tt
        else
          !OptEmpty(dispatch(p))
      case AddrOf(sub @ PointerArraySubscript(_, _)) => calculatePointer(sub)
      case AddrOf(ApplyCoercion(sub @ PointerArraySubscript(_, _), _)) =>
        calculatePointer(sub)
      case AddrOf(ApplyCoercion(inner, c)) =>
        applyCoercion(dispatch(inner), c) match {
          case PointerArraySubscript(_, _) =>
            throw Unreachable(
              "Unexpected non-identity coercions to PointerArraySubscript, missing case in EncodePointerArrays"
            )
          case _ => super.postCoerce(e)
        }
      // We only do AddrOf of non-null PointerArrays here since those have the address equal to their inner pointer. For nullable ones (i.e. in parameters) it should be the address of the parameter not the data
      case AddrOf(inner)
          if inner.t.asPointerArray.isDefined &&
            inner.t.asPointerArray.get.isNonNull =>
        val t = inner.t.asPointerArray.get
        initialiseAdt(t.element, t.dimensions.length, t.unique, t.isConst)
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
          constructors.getOrElseUpdate(
            (element, dimensions.length, unique, false),
            createConstructor(
              element,
              dimensions.length,
              unique,
              isConst = false,
            ),
          ).ref,
          dimensions.map(dispatch),
        )
      case npa @ NewConstPointerArray(element, dimensions) =>
        procedureInvocation(
          PointerArrayCreationFailed(npa, npa.blame),
          constructors.getOrElseUpdate(
            (element, dimensions.length, None, true),
            createConstructor(element, dimensions.length, None, isConst = true),
          ).ref,
          dimensions.map(dispatch),
        )
      case IntegerPointerCast(value, t, size)
          if value.t.asPointerArray.isDefined =>
        val arrayT = value.t.asPointerArray.get
        initialiseAdt(
          arrayT.element,
          arrayT.dimensions.length,
          arrayT.unique,
          arrayT.isConst,
        )
        IntegerPointerCast(
          adtFunctionInvocation(
            pointerSucc.ref((
              arrayT.element,
              arrayT.dimensions.length,
              arrayT.unique,
              arrayT.isConst,
            )),
            args = Seq(
              unwrapOption(value, NonNullCoercionBlame(globalBlame.top, e))
            ),
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
          case Local(Ref(v))
              if v.t.asPointerArray.isDefined &&
                currentVariableContext.add(v) =>
            variableHeapLabel(v) =
              if (currentStatementLabel.top.isEmpty) {
                val l = new LabelDecl[Post]()
                currentStatementLabel.top.addOne(l)
                l
              } else { currentStatementLabel.top.head }
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
    val (labels, newS) = currentStatementLabel.collect(s match {
      case Scope(variables, _) =>
        // Not adding variables until they're assigned since VerCors scopes don't match normal programming scopes since you can refer to a variable before its declaration
        val res = s.rewriteDefault()
        currentVariableContext --= variables
        res
      case s: AssignStmt[Pre] =>
        s.target match {
          case Local(Ref(v))
              if v.t.asPointerArray.isDefined &&
                currentVariableContext.add(v) =>
            val l = new LabelDecl[Post]()
            variableHeapLabel(v) = l
            Label(
              l,
              s.rewriteDefault(),
              LoopInvariant(tt, None)(TrueSatisfiable),
            )
          case _ => s.rewriteDefault()
        }
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
                getDimensionExpr() &* super.dispatch(invariant)
              )
            )
          case _: IterationContract[Pre] => throw ExtraNode
          case _: LLVMLoopContract[Pre] => throw ExtraNode
        }
      case bar: ParBarrier[Pre] =>
        bar.rewrite(
          requires = getDimensionExpr() &* dispatch(bar.requires),
          ensures = getDimensionExpr() &* dispatch(bar.ensures),
        )
      case frame: FramedProof[Pre] =>
        frame.rewrite(
          pre = getDimensionExpr() &* dispatch(frame.pre),
          post = getDimensionExpr() &* dispatch(frame.post),
        )
      case _ => super.postCoerce(s)
    })
    if (labels.isEmpty) { newS }
    else { Label(labels.head, newS, LoopInvariant(tt, None)(TrueSatisfiable)) }
  }

  override def postCoerce(parRegion: ParRegion[Pre]): ParRegion[Post] = {
    implicit val o: Origin = parRegion.o

    parRegion match {
      case block: ParBlock[Pre] =>
        block.rewrite(
          requires = getDimensionExpr() &* dispatch(block.requires),
          ensures = getDimensionExpr() &* dispatch(block.ensures),
        )
      case _: ParParallel[Pre] | _: ParSequential[Pre] =>
        parRegion.rewriteDefault()
    }
  }

  private def getDimensionExpr(
      useOld: Boolean = true
  )(implicit o: Origin): Expr[Post] = {
    foldAnd(
      currentVariableContext.flatMap(v =>
        v.t.asPointerArray.map((v, _, variableHeapLabel.get(v).map(_.ref)))
      ).flatMap { case (v, t, l) =>
        implicit val o: Origin = v.o.where(context = "Dimension invariant")
        val dimensions = t.dimensions.length
        initialiseAdt(t.element, dimensions, t.unique, t.isConst)

        val calcDim =
          (newV: Expr[Post]) => {
            t.dimensions.zipWithIndex.filter { case (d, _) => d.isDefined }
              .map { case (d, i) =>
                adtFunctionInvocation[Post](
                  dimSucc.ref((t.element, dimensions, t.unique, i, t.isConst)),
                  args = Seq(newV),
                ) ===
                  (if (
                     useOld &&
                     !ExpressionEqualityCheck.stricterIsConstant(d.get)
                   ) {
                     Old(dispatch(d.get), l)(PanicBlame(
                       "Rewrite order should ensure that we only add dimension expressions for variables that have been initialised. This program probably contains some sort of control flow we were not expecting"
                     ))
                   } else { dispatch(d.get) })
              }
          }

        if (t.isNonNull) { calcDim(Local(succ(v))) }
        else {
          calcDim(OptGet[Post](Local(succ(v)))(PanicBlame(
            "Can never be null since this is ensured in the conditional expression"
          ))).map(!OptEmpty(Local[Post](succ(v))) ==> _)
        }
      }
    )
  }

  override def postCoerce(t: Type[Pre]): Type[Post] =
    t match {
      case a: PointerArrayType[Pre] =>
        val axiomType = TAxiomatic[Post](
          initialiseAdt(a.element, a.dimensions.length, a.unique, a.isConst)
            .ref,
          Nil,
        )
        if (a.isNonNull) { axiomType }
        else { TOption(axiomType) }
      case _ => super.postCoerce(t)
    }

  override def postCoerce(decl: Declaration[Pre]): Unit = {
    implicit val o: Origin = decl.o
    decl match {
      case app: ContractApplicable[Pre] =>
        val requires =
          (oldRequires: AccountedPredicate[Post]) =>
            app.args.flatMap { v => v.t.asPointerArray.map((v, _)) }
              .flatMap[UnitAccountedPredicate[Post]]({ case (v, t) =>
                val dimensions = t.dimensions.length
                initialiseAdt(t.element, dimensions, t.unique, t.isConst)

                val calcDim =
                  (newV: Expr[Post]) => {
                    t.dimensions.zipWithIndex.filter { case (d, _) =>
                      d.isDefined
                    }.map { case (d, i) =>
                      adtFunctionInvocation[Post](
                        dimSucc
                          .ref((t.element, dimensions, t.unique, i, t.isConst)),
                        args = Seq(newV),
                      ) === dispatch(d.get)
                    }
                  }

                (if (t.isNonNull) { calcDim(Local(succ(v))) }
                 else {
                   calcDim(OptGet[Post](Local(succ(v)))(PanicBlame(
                     "Can never be null since this is ensured in the conditional expression"
                   ))).map(!OptEmpty(Local[Post](succ(v))) ==> _)
                 }).map(UnitAccountedPredicate(_))
              }).foldLeft(oldRequires) { case (r, l) =>
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
          p.rewrite(body =
            Some(getDimensionExpr(useOld = false) &* dispatch(p.body.get))
          ),
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
        args = Seq(unwrapOption(obj, sub.blame)),
      ),
      index,
    )(CalculatedPointerAddBlame(sub.blame))
  }

  private def calculateOffset(
      sub: PointerArraySubscript[Pre],
      depth: Int,
  ): (Expr[Pre], Expr[Post], Int) = {
    implicit val o: Origin = sub.o
    val arrayT = sub.array.t.asPointerArray.get
    val (obj, index, length) =
      sub.array match {
        case p: InlinePattern[Pre] => throw InvalidPatternLocation(p)
        case inner: PointerArraySubscript[Pre] =>
          calculateOffset(inner, depth + 1)
        case other => (other, const[Post](0), arrayT.dimensions.length)
      }
    val newIndex = {
      Seq.range(length - depth + 1, length).map(i =>
        adtFunctionInvocation[Post](
          dimSucc.ref(arrayT.element, length, arrayT.unique, i, arrayT.isConst),
          args = Seq(unwrapOption(obj, sub.blame)),
        )
      ).fold(super.dispatch(sub.index))(Mult(_, _)) + index
    }
    (obj, newIndex, length)
  }

  private def initialiseAdt(
      element: Type[Pre],
      dimensions: Int,
      unique: Option[BigInt],
      isConst: Boolean,
  ): AxiomaticDataType[Post] = {
    arraySucc.getOrElseUpdate(
      (element, dimensions, unique, isConst), {
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
              ) === dimFunctions.map(f =>
                adtFunctionInvocation[Post](f.ref, args = Seq(term))
              ).reduce((a: Expr[Post], b: Expr[Post]) => Mult(a, b))
            },
          ))
        val dimAxioms = dimFunctions.map(f =>
          new ADTAxiom[Post](forall(
            axiomType,
            { term =>
              InlinePattern(
                adtFunctionInvocation[Post](f.ref, args = Seq(term))
              ) > const(0)
            },
          ))
        )
        globalDeclarations.declare(
          new AxiomaticDataType(
            dimFunctions ++
              Seq(pointerFunction, invFunction, invAxiom, boundsAxiom) ++
              dimAxioms,
            Nil,
          )(o.where(name =
            if (isConst) { s"const_pointer_${dimensions}_array_$element" }
            else { s"pointer_${dimensions}_array_$element" }
          ))
        )
      },
    )
  }

  private def createConstructor(
      element: Type[Pre],
      dimensions: Int,
      unique: Option[BigInt],
      isConst: Boolean,
  ): Procedure[Post] = {
    implicit val o: Origin = ConstructorOrigin
    initialiseAdt(element, dimensions, unique, isConst)
    val axiomType = TAxiomatic[Post](
      arraySucc.ref((element, dimensions, unique, isConst)),
      Nil,
    )
    val dimFunctions: Seq[Ref[Post, ADTFunction[Post]]] = Seq
      .range(0, dimensions)
      .map(i => dimSucc.ref((element, dimensions, unique, i, isConst)))
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
          val ptr =
            () =>
              adtFunctionInvocation[Post](
                pointerSucc.ref((element, dimensions, unique, isConst)),
                args = Seq(result),
              )
          val trigger =
            (term: Local[Post]) =>
              PointerSubscript(ptr(), term)(FramedPtrOffset)
          val bounds =
            foldAnd(dimFunctions.zip(args).map { case (f, a) =>
              adtFunctionInvocation[Post](f, args = Seq(result)) === a.get
            }) && PointerBlockLength(ptr())(NonNullPointerNull) === max &&
              PointerBlockOffset(ptr())(NonNullPointerNull) === const(0)
          val basePerms =
            if (isConst)
              bounds
            else {
              bounds &* starall(
                IteratedPtrInjective,
                TInt(),
                body = { term =>
                  range(term) ==> Perm(
                    PointerLocation(PointerAdd(ptr(), term)(FramedPtrOffset))(
                      NonNullPointerNull
                    ),
                    WritePerm(),
                  )
                },
                triggers = t => Seq(Seq(trigger(t))),
              )
            }
          val fullPerms =
            if (element.asByValueClass.isDefined) {
              basePerms &* starall(
                IteratedPtrInjective,
                TInt(),
                body = { term =>
                  range(term) ==> Perm(
                    ByValueClassLocation(
                      PointerSubscript(ptr(), term)(FramedPtrOffset)
                    ),
                    WritePerm(),
                  )
                },
                triggers = t => Seq(Seq(trigger(t))),
              )
            } else { basePerms }
          UnitAccountedPredicate(fullPerms)
        },
      )(o.where(name =
        s"create_${if (isConst)
            "const_"
          else { "" }}${dimensions}_array$element"
      ))
    }))
  }
}
