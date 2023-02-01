package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.{Blame, DiagnosticOrigin, FrontendAdditiveError, InstanceInvocationFailure, InstanceNull, InvocationFailure, Origin, PlusProviderInvocationFailed, PlusProviderNull, WithContractFailure}
import vct.col.ref.Ref
import vct.col.rewrite.Disambiguate.OperatorToInvocation
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.typerules.CoercionUtils
import vct.col.util.AstBuildHelpers.withResult
import vct.col.util.SuccessionMap
import vct.result.VerificationError.Unreachable

case object Disambiguate extends RewriterBuilder {
  override def key: String = "disambiguate"
  override def desc: String = "Translate ambiguous operators into concrete operators."

  case class OperatorToInvocation(blame: Blame[FrontendAdditiveError]) extends Blame[InstanceInvocationFailure] {
    override def blame(error: InstanceInvocationFailure): Unit = error match {
      case InstanceNull(node) => blame.blame(PlusProviderNull(node))
      case failure: WithContractFailure => blame.blame(PlusProviderInvocationFailed(failure))
    }
  }
}

case class Disambiguate[Pre <: Generation]() extends Rewriter[Pre] {
  val functionSucc: SuccessionMap[InstanceOperatorFunction[Pre], InstanceFunction[Post]] = SuccessionMap()
  val methodSucc: SuccessionMap[InstanceOperatorMethod[Pre], InstanceMethod[Post]] = SuccessionMap()

  val currentResult: ScopedStack[Result[Post]] = ScopedStack()

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case f: InstanceOperatorFunction[Pre] =>
      functionSucc(f) = withResult { result: Result[Post] =>
        currentResult.having(result) {
          classDeclarations.declare(new InstanceFunction[Post](
            dispatch(f.returnType),
            variables.collect(f.args.map(dispatch(_)))._1,
            variables.collect(f.typeArgs.map(dispatch(_)))._1,
            f.body.map(dispatch(_)),
            dispatch(f.contract),
            f.inline,
            f.threadLocal
          )(f.blame)(f.o))
        }
      }(DiagnosticOrigin)
    case m: InstanceOperatorMethod[Pre] =>
      methodSucc(m) = withResult { result: Result[Post] =>
        currentResult.having(result) {
          classDeclarations.declare(new InstanceMethod[Post](
            dispatch(m.returnType),
            variables.collect(m.args.map(dispatch(_)))._1,
            variables.collect(m.outArgs.map(dispatch(_)))._1,
            variables.collect(m.typeArgs.map(dispatch(_)))._1,
            m.body.map(dispatch(_)),
            dispatch(m.contract),
            m.inline,
            m.pure
          )(m.blame)(m.o))
        }
      }(DiagnosticOrigin)
    case _ => rewriteDefault(decl)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case op @ AmbiguousMult(left, right) =>
        if(op.isProcessOp) ProcessSeq(dispatch(left), dispatch(right))
        else if(op.isSetOp) SetIntersection(dispatch(left), dispatch(right))
        else if(op.isBagOp) BagLargestCommon(dispatch(left), dispatch(right))
        else Mult(dispatch(left), dispatch(right))
      case op @ AmbiguousPlus(left, right) =>
        if(op.isProcessOp) ProcessChoice(dispatch(left), dispatch(right))
        else if(op.isPointerOp) unfoldPointerAdd(PointerAdd(dispatch(left), dispatch(right))(op.blame))
        else if(op.isSeqOp) Concat(dispatch(left), dispatch(right))
        else if(op.isSetOp) SetUnion(dispatch(left), dispatch(right))
        else if(op.isBagOp) BagAdd(dispatch(left), dispatch(right))
        else if(op.isStringOp) StringConcat(dispatch(left), dispatch(right))
        else if(op.getCustomLeftPlusType().isDefined) rewriteLeftPlus(op)
        else if(op.getCustomRightPlusType().isDefined) rewriteRightPlus(op)
        else Plus(dispatch(left), dispatch(right))
      case op @ AmbiguousMinus(left, right) =>
        if(op.isSetOp) SetMinus(dispatch(left), dispatch(right))
        else if(op.isPointerOp) PointerAdd(dispatch(left), dispatch(UMinus(right)))(op.blame)
        else if(op.isBagOp) BagMinus(dispatch(left), dispatch(right))
        else Minus(dispatch(left), dispatch(right))
      case op @ AmbiguousOr(left, right) =>
        if(op.isProcessOp) ProcessPar(dispatch(left), dispatch(right))
        else Or(dispatch(left), dispatch(right))
      case op: BitOp[Pre] =>
        val cons = if(op.isBoolOp) op match {
          case _: AmbiguousComputationalOr[Pre] => Or[Post](_, _)
          case _: AmbiguousComputationalXor[Pre] => Neq[Post](_, _)
          case _: AmbiguousComputationalAnd[Pre] => And[Post](_, _)
        } else op match {
          case _: AmbiguousComputationalOr[Pre] => ComputationalOr[Post](_, _)
          case _: AmbiguousComputationalXor[Pre] => ComputationalXor[Post](_, _)
          case _: AmbiguousComputationalAnd[Pre] => ComputationalAnd[Post](_, _)
        }

        cons(dispatch(op.left), dispatch(op.right))
      case op @ AmbiguousSubscript(collection, index) =>
        if(op.isPointerOp) PointerSubscript(dispatch(collection), dispatch(index))(op.blame)
        else if(op.isMapOp) MapGet(dispatch(collection), dispatch(index))(op.blame)
        else if(op.isArrayOp) ArraySubscript(dispatch(collection), dispatch(index))(op.blame)
        else if(op.isSeqOp) SeqSubscript(dispatch(collection), dispatch(index))(op.blame)
        else throw Unreachable("AmbiguousSubscript must subscript a pointer, map, array, or seq because of the type check.")
      case op @ AmbiguousMember(x, xs) =>
        if(op.isMapOp) MapMember(dispatch(x), dispatch(xs))
        else if(op.isSetOp) SetMember(dispatch(x), dispatch(xs))
        else if(op.isBagOp) BagMemberCount(dispatch(x), dispatch(xs))
        else if(op.isSeqOp) SeqMember(dispatch(x), dispatch(xs))
        else throw Unreachable("AmbiguousMember must query a map, set, bag, or seq because of the type check.")
      case cmp: AmbiguousOrderOp[Pre] =>
        if(cmp.isBagOp) cmp match {
          case AmbiguousGreater(left, right) => SubBag(dispatch(right), dispatch(left))
          case AmbiguousLess(left, right) => SubBag(dispatch(left), dispatch(right))
          case AmbiguousGreaterEq(left, right) => SubBagEq(dispatch(right), dispatch(left))
          case AmbiguousLessEq(left, right) => SubBagEq(dispatch(left), dispatch(right))
        } else if(cmp.isSetOp) cmp match {
          case AmbiguousGreater(left, right) => SubSet(dispatch(right), dispatch(left))
          case AmbiguousLess(left, right) => SubSet(dispatch(left), dispatch(right))
          case AmbiguousGreaterEq(left, right) => SubSetEq(dispatch(right), dispatch(left))
          case AmbiguousLessEq(left, right) => SubSetEq(dispatch(left), dispatch(right))
        } else cmp match {
          case AmbiguousGreater(left, right) => Greater(dispatch(left), dispatch(right))
          case AmbiguousLess(left, right) => Less(dispatch(left), dispatch(right))
          case AmbiguousGreaterEq(left, right) => GreaterEq(dispatch(left), dispatch(right))
          case AmbiguousLessEq(left, right) => LessEq(dispatch(left), dispatch(right))
        }
      case r @ Result(_) if currentResult.nonEmpty => Result(currentResult.top.applicable)(r.o)
      case other => rewriteDefault(other)
    }
  }

  def rewriteLeftPlus(plus: AmbiguousPlus[Pre]): Expr[Post] = {
    val left = plus.left
    val right = plus.right
    val decls = left.t match {
      case TClass(Ref(cls)) => cls.declarations
      case JavaTClass(Ref(cls), _) => cls.declarations
    }
    val validOperators = decls.collect {
      case m: InstanceOperatorMethod[Pre] if m.operator == OperatorLeftPlus[Pre]()
        && CoercionUtils.getCoercion(right.t, m.args.head.t).isDefined => m
      case f: InstanceOperatorFunction[Pre] if f.operator == OperatorLeftPlus[Pre]()
        && CoercionUtils.getCoercion(right.t, f.args.head.t).isDefined => f
    }
    validOperators match {
      case Seq(m: InstanceOperatorMethod[Pre]) => MethodInvocation[Post](
        dispatch(left),
        methodSucc.ref(m),
        Seq(dispatch(right)),
        Seq(), Seq(), Seq(), Seq())(OperatorToInvocation(plus.blame))(plus.o)
      case Seq(f: InstanceOperatorFunction[Pre]) => InstanceFunctionInvocation[Post](
        dispatch(left),
        functionSucc.ref(f),
        Seq(dispatch(right)),
        Seq(), Seq(), Seq())(OperatorToInvocation(plus.blame))(plus.o)
      case _ => ???
    }
  }

  def rewriteRightPlus(plus: AmbiguousPlus[Pre]): Expr[Post] = {
    ???
  }

  def unfoldPointerAdd[G](e: PointerAdd[G]): PointerAdd[G] = e.pointer match {
    case inner @ PointerAdd(_, _) =>
    val PointerAdd(pointerInner, offsetInner) = unfoldPointerAdd(inner)
    PointerAdd(pointerInner, Plus(offsetInner, e.offset)(e.o) )(e.blame)(e.o)
    case _ => e
  }
}
