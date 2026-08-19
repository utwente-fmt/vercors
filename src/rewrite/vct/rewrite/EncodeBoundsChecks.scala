package vct.rewrite

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.expr.op.BinOperatorTypes
import vct.col.ast.{
  AbstractFunction,
  AbstractMethod,
  AccountedPredicate,
  AmbiguousBitShr,
  AmbiguousDiv,
  AmbiguousDividingExpr,
  AmbiguousMinus,
  AmbiguousMod,
  AmbiguousMult,
  AmbiguousOr,
  AmbiguousPlus,
  AmbiguousTruncDiv,
  AmbiguousTruncMod,
  And,
  AnyFunctionInvocation,
  AnyMethodInvocation,
  ApplicableContract,
  Assert,
  Asserting,
  AssignExpression,
  AssignStmt,
  Assume,
  Assuming,
  BinExpr,
  Binder,
  BitAnd,
  BitNot,
  BitOp,
  BitOr,
  BitShl,
  BitShr,
  BitUShr,
  BitXor,
  Cast,
  CheckedIntegerValue,
  CoerceCheckedIntInt,
  Coercion,
  Comparison,
  ComputationalAnd,
  ComputationalOr,
  ComputationalXor,
  ConstructorInvocation,
  ContractApplicable,
  Declaration,
  Deref,
  DerefPointer,
  DividingExpr,
  DividingVectorBinExpr,
  Exhale,
  Exists,
  Exp,
  Expr,
  FloatDiv,
  FloorDiv,
  ForPerm,
  ForPermWithValue,
  Forall,
  FunctionInvocation,
  Implies,
  Inhale,
  InstanceFunctionInvocation,
  IntegerValue,
  Invocation,
  InvocationStatement,
  InvokeConstructor,
  InvokeMethod,
  InvokeProcedure,
  IterationContract,
  LLVMLoopContract,
  Let,
  Local,
  LocalDecl,
  Location,
  LoopContract,
  LoopInvariant,
  MatrixCompare,
  MethodInvocation,
  Minus,
  Mod,
  Mult,
  Node,
  Not,
  NumericBinExpr,
  Or,
  Plus,
  Predicate,
  ProcedureInvocation,
  Product,
  RatDiv,
  Result,
  Scope,
  SmtlibPow,
  SplitAccountedPredicate,
  Starall,
  Statement,
  StringConcat,
  Sum,
  TBoundedInt,
  TCheckedInt,
  TInt,
  TruncDiv,
  TruncMod,
  Type,
  TypeValue,
  UMinus,
  UnExpr,
  UncheckedMath,
  UnitAccountedPredicate,
  Variable,
  VectorBinExpr,
  VectorCompare,
}
import vct.col.origin.{
  AssertFailed,
  Blame,
  CallOutOfBounds,
  ContractedFailure,
  IntegerOutOfBounds,
  IntegerOverflow,
  IntegerUnderflow,
  Origin,
  PostBlameSplit,
  PostconditionFailed,
  PreBlameSplit,
  PreconditionFailed,
  ReturnOutOfBounds,
  VerificationFailure,
  YieldsOutOfBounds,
}
import vct.col.ref.Ref
import vct.col.rewrite.error.ExtraNode
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.typerules.CoercionUtils
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.{Unreachable, UserError}

import scala.annotation.tailrec
import scala.collection.mutable

case object EncodeBoundsChecks extends RewriterBuilder {

  override def key: String = "boundsChecks"

  override def desc: String =
    "Encodes bounds checks for every expression involving a TCheckedInt type"

  private case class UnderflowBlame(
      expr: Expr[_],
      blame: Blame[IntegerOutOfBounds],
      gte: BigInt,
  ) extends Blame[AssertFailed] {

    override def blame(error: AssertFailed): Unit =
      blame.blame(IntegerUnderflow(expr, gte))
  }

  private case class OverflowBlame(
      expr: Expr[_],
      blame: Blame[IntegerOutOfBounds],
      lt: BigInt,
  ) extends Blame[AssertFailed] {

    override def blame(error: AssertFailed): Unit =
      blame.blame(IntegerOverflow(expr, lt))
  }

  private case class ReturnOutOfBoundsBlame(
      node: ContractApplicable[_],
      e: Expr[_],
      blame: Blame[IntegerOutOfBounds],
      gte: BigInt,
      lt: BigInt,
  ) extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit =
      e match {
        case Result(_) => blame.blame(ReturnOutOfBounds(node, gte, lt))
        case Local(Ref(v)) => blame.blame(YieldsOutOfBounds(node, v, gte, lt))
      }
  }

  private case class CallOutOfBoundsBlame(
      inv: Node[_],
      v: Variable[_],
      e: Expr[_],
      blame: Blame[IntegerOutOfBounds],
      gte: BigInt,
      lt: BigInt,
  ) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      blame.blame(CallOutOfBounds(inv, v, e, gte, lt))
  }

  private case class MissingGiven(inv: Node[_], v: Variable[_])
      extends UserError {
    override def code: String = "missingGiven"

    override def text: String =
      inv.o.messageInContext(s"Missing assignment for given variable $v")
  }

  private case class ConstantOutOfBounds(c: CheckedIntegerValue[_])
      extends UserError {
    override def code: String = "constantBounds"
    override def text: String =
      c.o.messageInContext(
        s"Expected this constant to be in the range [${c.gte},${c.lt})"
      )
  }
}

case class EncodeBoundsChecks[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging {
  import EncodeBoundsChecks._

  private val inPure: ScopedStack[Unit] = ScopedStack()
  private val inLocation: ScopedStack[Unit] = ScopedStack()
  private val currentVars: mutable.HashMap[Variable[Pre], (BigInt, BigInt)] =
    mutable.HashMap()

  // Assumption! (AS): We are assuming these are "regular" integer bounds i.e. 0..((2^N) -1) or -(2^N)..((2^N) -1)
  //                   This is important because we assume lower-bound < 0 imply a signed number and for div and uminus
  //                   we only need to check if we are at the lower bound to see if there will be an overflow

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case UncheckedMath(inner) => inPure.having(()) { dispatch(inner) }
      case p: AmbiguousPlus[Pre] if inPure.isEmpty => checkPost(p)
      case p: Plus[Pre] if inPure.isEmpty => checkPost(p)
      case m: AmbiguousMinus[Pre] if inPure.isEmpty => checkPost(m)
      case m: Minus[Pre] if inPure.isEmpty => checkPost(m)
      case div: AmbiguousDividingExpr[Pre] if inPure.isEmpty =>
        div match {
          case _: AmbiguousDiv[Pre] | _: AmbiguousTruncDiv[Pre] =>
            if (div.isVectorOp)
              return super.dispatch(div)
            checkDivision(div)
          case _: AmbiguousMod[Pre] | _: AmbiguousTruncMod[Pre] =>
            super.dispatch(e)
        }
      case div: FloorDiv[Pre] if inPure.isEmpty => checkDivision(div)
      case div: TruncDiv[Pre] if inPure.isEmpty => checkDivision(div)
      case m: AmbiguousMult[Pre] if inPure.isEmpty => checkPost(m)
      case m: Mult[Pre] if inPure.isEmpty => checkPost(m)
      case UMinus(e) if inPure.isEmpty =>
        e.t match {
          case i @ TCheckedInt(gte, lt) if gte < 0 =>
            Asserting[Post](
              const[Post](gte) <= inPure.having(()) { dispatch(e) },
              UMinus(dispatch(e)),
            )(OverflowBlame(e, i.blame, lt))
          case _ => UMinus(dispatch(e))
        }
      case _: Exp[Pre] | _: SmtlibPow[Pre] if inPure.isEmpty =>
        logger.warn("Integer bounds are not checked on power operators")
        super.dispatch(e)
      case _: AmbiguousOr[Pre] | _: BitOp[_] | _: ComputationalOr[Pre] |
          _: ComputationalXor[Pre] | _: ComputationalAnd[Pre] | _: RatDiv[Pre] |
          _: FloatDiv[Pre] | _: Mod[Pre] | _: TruncMod[Pre] |
          _: VectorBinExpr[_] | _: StringConcat[Pre] | _: BitAnd[Pre] |
          _: BitOr[Pre] | _: BitXor[Pre] | _: BitShl[Pre] |
          _: AmbiguousBitShr[Pre] | _: BitShr[Pre] | _: BitUShr[Pre] |
          _: And[Pre] | _: Or[Pre] | _: Implies[Pre] | _: Comparison[_] |
          _: VectorCompare[Pre] | _: MatrixCompare[Pre] | _: BitNot[Pre] |
          _: Not[Pre] =>
        super.dispatch(e)
      // If we fall through here there was some operator we forgot to implement here
      case _: BinExpr[Pre] | _: UnExpr[Pre] | _: DividingExpr[Pre]
          if inPure.isEmpty =>
        throw ExtraNode
      case c @ CheckedIntegerValue(value, gte, lt) =>
        if (value >= gte && value < lt)
          IntegerValue(value)
        else
          throw ConstantOutOfBounds(c)
      case inv: Invocation[Pre] =>
        val checkedArgs = getInvocationArgs(
          inv,
          inv.args,
          inv.givenMap,
          inv.ref.decl,
        )
        inv match {
          case inv: AnyMethodInvocation[Pre] =>
            inv match {
              case inv: ProcedureInvocation[Pre] =>
                inv.rewrite(blame =
                  addPreBlameSplit(inv, checkedArgs, inv.blame)
                )
              case inv: MethodInvocation[Pre] =>
                inv.rewrite(blame =
                  addPreBlameSplit(inv, checkedArgs, inv.blame)
                )
              case inv: ConstructorInvocation[Pre] =>
                inv.rewrite(blame =
                  addPreBlameSplit(inv, checkedArgs, inv.blame)
                )
            }
          case inv: AnyFunctionInvocation[Pre] =>
            inv match {
              case inv: FunctionInvocation[Pre] =>
                inv.rewrite(blame =
                  addPreBlameSplit(inv, checkedArgs, inv.blame)
                )
              case inv: InstanceFunctionInvocation[Pre] =>
                inv.rewrite(blame =
                  addPreBlameSplit(inv, checkedArgs, inv.blame)
                )
            }
        }
      case Cast(e, TypeValue(TCheckedInt(_, _))) => dispatch(e)
      // Primarily to catch casts from TBoundedInt to TInt
      case Cast(e, TypeValue(TInt()))
          if CoercionUtils.getAnyCoercion(e.t, TInt()).isDefined =>
        dispatch(e)
      // TODO: Can we deduplicate here?
      case Deref(obj, Ref(f)) if inLocation.isEmpty =>
        f.t match {
          case TCheckedInt(gte, lt) =>
            let(
              dispatch(f.t),
              super.dispatch(e),
              x => Assuming(x >= const(gte) && x < const(lt), x),
            )
          case _ => super.dispatch(e)
        }
      case DerefPointer(p) if inLocation.isEmpty =>
        e.t match {
          case TCheckedInt(gte, lt) =>
            let(
              dispatch(e.t),
              super.dispatch(e),
              x => Assuming(x >= const(gte) && x < const(lt), x),
            )
          case _ => super.dispatch(e)
        }
      case asserting @ Asserting(condition, _) =>
        asserting.rewrite(condition = inPure.having(()) { dispatch(condition) })
      case assuming @ Assuming(assn, _) =>
        assuming.rewrite(assn = inPure.having(()) { dispatch(assn) })
      case binder: Binder[Pre] =>
        binder match {
          case _: Exists[Pre] | _: Forall[Pre] | _: Starall[Pre] |
              _: ForPerm[Pre] | _: ForPermWithValue[Pre] | _: Sum[Pre] |
              _: Product[Pre] =>
            inPure.having(()) { super.dispatch(binder) }
          case let @ Let(_, _, _) => super.dispatch(let)
        }
      case a: AssignExpression[Pre] =>
        a.target match {
          case WithExactType(Local(Ref(v)), TCheckedInt(gte, lt)) =>
            currentVars(v) = (gte, lt)
          case _ =>
        }
        super.dispatch(a)
      case _ => super.dispatch(e)
    }
  }

  override def dispatch(l: Location[Pre]): Location[Post] = {
    inLocation.having(()) { super.dispatch(l) }
  }

  private def checkPost(op: BinExpr[Pre])(implicit o: Origin): Expr[Post] = {
    if (op.isVectorOp)
      return super.dispatch(op)
    op.t match {
      case i @ TCheckedInt(gte, lt) =>
        Asserting(
          inPure.having(()) { super.dispatch(op) } >= const(gte),
          Asserting(
            inPure.having(()) { super.dispatch(op) } < const(lt),
            super.dispatch(op),
          )(OverflowBlame(op, i.blame, lt)),
        )(UnderflowBlame(op, i.blame, gte))
      case _ => super.dispatch(op)
    }
  }

  private def checkDivision(e: BinExpr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e.t match {
      case i @ TCheckedInt(gte, lt) if gte < 0 =>
        Asserting(
          inPure.having(()) { dispatch(e.left) } > const(gte) ||
            inPure.having(()) { dispatch(e.right) } >= const(0),
          super.dispatch(e),
        )(OverflowBlame(e, i.blame, lt))
      case _ => super.dispatch(e)
    }
  }

  private def havingVars[T](vs: Seq[Variable[Pre]])(op: => T): T = {
    val filtered = vs.filter(_.t.isInstanceOf[TCheckedInt[Pre]])
    filtered.foreach { v =>
      val t = v.t.asInstanceOf[TCheckedInt[Pre]]
      currentVars(v) = (t.gte, t.lt)
    }
    val res = op
    filtered.foreach { v =>
      val t = v.t.asInstanceOf[TCheckedInt[Pre]]
      currentVars.remove(v)
    }
    res
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    implicit val o: Origin = decl.o
    decl match {
      case c: ContractApplicable[Pre] =>
        havingVars(c.args ++ c.contract.givenArgs ++ c.contract.yieldsArgs) {
          val checkedArgs = (c.args ++ c.contract.givenArgs)
            .map(a => a.get(a.o))
          var checkedRets =
            Result[Pre](c.ref)(c.o) +:
              c.contract.yieldsArgs.map(a => a.get(a.o))

          val res =
            c match {
              case function: AbstractFunction[_] =>
                function.rewrite(
                  contract =
                    inPure.having(()) {
                      c.contract.rewrite(
                        requires = addChecks(checkedArgs, c.contract.requires),
                        ensures = addChecks(checkedRets, c.contract.ensures),
                      )
                    },
                  blame = addPostBlameSplit(c, checkedRets, function.blame),
                )
              case method: AbstractMethod[_] =>
                method.rewrite(
                  contract =
                    inPure.having(()) {
                      c.contract.rewrite(
                        requires = addChecks(checkedArgs, c.contract.requires),
                        ensures = addChecks(checkedRets, c.contract.ensures),
                      )
                    },
                  blame = addPostBlameSplit(c, checkedRets, method.blame),
                )
            }
          allScopes.anySucceed(c, res)
        }

      case p: Predicate[Pre] if p.body.isDefined =>
        havingVars(p.args) {
          globalDeclarations.succeed(
            p,
            p.rewrite(body =
              Some(
                getChecks().map(_ &* inPure.having(()) { dispatch(p.body.get) })
                  .getOrElse(dispatch(p.body.get))
              )
            ),
          )
        }
      case _ => super.dispatch(decl)
    }
  }

  private def addChecks(
      vars: Seq[Expr[Pre]],
      pred: AccountedPredicate[Pre],
  ): AccountedPredicate[Post] =
    if (vars.isEmpty) { dispatch(pred) }
    else {
      implicit val o: Origin = vars.head.o
      val newVar = dispatch(vars.head)
      vars.head.t match {
        case TCheckedInt(gte, lt) =>
          SplitAccountedPredicate(
            UnitAccountedPredicate(
              (newVar >= const(gte)) && (newVar < const(lt))
            ),
            addChecks(vars.tail, pred),
          )(pred.o)
        case _ => addChecks(vars.tail, pred)
      }
    }

  @tailrec
  private def addPostBlameSplit[
      T >: PostconditionFailed <: VerificationFailure
  ](
      node: ContractApplicable[_],
      vars: Seq[Expr[Pre]],
      blame: Blame[T],
  ): Blame[T] =
    if (vars.isEmpty) { blame }
    else {
      vars.head.t match {
        case i @ TCheckedInt(gte, lt) =>
          PostBlameSplit.left(
            ReturnOutOfBoundsBlame(node, vars.head, i.blame, gte, lt),
            blame,
          )
        case _ => addPostBlameSplit(node, vars.tail, blame)
      }
    }

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case inv: InvocationStatement[Pre] =>
        val checkedArgs = getInvocationArgs(
          inv,
          inv.args,
          inv.givenMap,
          inv.ref.decl,
        )
        inv match {
          case p: InvokeProcedure[Pre] =>
            p.rewrite(blame = addPreBlameSplit(inv, checkedArgs, p.blame))
          case c: InvokeConstructor[Pre] =>
            c.rewrite(blame = addPreBlameSplit(inv, checkedArgs, c.blame))
          case m: InvokeMethod[Pre] =>
            m.rewrite(blame = addPreBlameSplit(inv, checkedArgs, m.blame))
        }
      case s @ Scope(vars, body) =>
        val res = s.rewriteDefault()
        (vars ++ body.collect { case LocalDecl(v) => v })
          .foreach(currentVars.remove)
        res
      case assert @ Assert(res) =>
        assert.rewrite(res = inPure.having(()) { dispatch(res) })
      case exhale @ Exhale(res) =>
        exhale.rewrite(res = inPure.having(()) { dispatch(res) })
      case inhale @ Inhale(res) =>
        inhale.rewrite(res = inPure.having(()) { dispatch(res) })
      case assume @ Assume(assn) =>
        assume.rewrite(assn = inPure.having(()) { dispatch(assn) })
      case a: AssignStmt[Pre] =>
        a.target match {
          case WithExactType(Local(Ref(v)), TCheckedInt(gte, lt)) =>
            currentVars(v) = (gte, lt)
          case _ =>
        }
        super.dispatch(a)
      case _ => super.dispatch(stat)
    }

  @tailrec
  private def addPreBlameSplit[T >: PreconditionFailed <: VerificationFailure](
      node: Node[_],
      vars: Seq[(Variable[Pre], Expr[Pre])],
      blame: Blame[T],
  ): Blame[T] =
    if (vars.isEmpty) { blame }
    else {
      vars.head._1.t match {
        case i @ TCheckedInt(gte, lt) =>
          PreBlameSplit.left(
            CallOutOfBoundsBlame(
              node,
              vars.head._1,
              vars.head._2,
              i.blame,
              gte,
              lt,
            ),
            blame,
          )
        case _ => addPreBlameSplit(node, vars.tail, blame)
      }
    }

  private def getInvocationArgs(
      inv: Node[Pre],
      args: Seq[Expr[Pre]],
      givenMap: Seq[(Ref[Pre, Variable[Pre]], Expr[Pre])],
      c: ContractApplicable[Pre],
  ): Seq[(Variable[Pre], Expr[Pre])] =
    c.args.zip(args) ++ c.contract.givenArgs.map(v =>
      (
        v,
        givenMap.find { case (Ref(r), e) => r == v }
          .getOrElse(throw MissingGiven(inv, v))._2,
      )
    )

  override def dispatch(t: Type[Pre]): Type[Post] =
    t match {
      case TCheckedInt(_, _) => TInt()
      case _ => super.dispatch(t)
    }

  private def getChecks()(implicit o: Origin): Option[Expr[Post]] =
    currentVars.map { case (v, (gte, lte)) =>
      Local[Post](succ(v)) >= const(gte) && Local[Post](succ(v)) < const(lte)
    }.reduceOption(And[Post](_, _))

  override def dispatch(node: LoopContract[Pre]): LoopContract[Post] = {
    implicit val o: Origin = node.o
    node match {
      case i @ LoopInvariant(invariant, _) =>
        inPure.having(()) {
          i.rewrite(invariant =
            getChecks().map(_ &* dispatch(invariant))
              .getOrElse(dispatch(invariant))
          )
        }
      case c @ IterationContract(requires, ensures, _) =>
        inPure.having(()) {
          c.rewrite(
            requires = getChecks().map(_ &* dispatch(requires))
              .getOrElse(dispatch(requires)),
            ensures = getChecks().map(_ &* dispatch(ensures))
              .getOrElse(dispatch(ensures)),
          )
        }
      case contract: LLVMLoopContract[Pre] => throw ExtraNode
    }
  }
}
