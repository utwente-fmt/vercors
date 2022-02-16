package vct.col.newrewrite

import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import RewriteHelpers._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationResult.UserError

case object IterationContractToParBlock extends RewriterBuilder {
  case object IterationContractOrigin extends Origin {
    override def preferredName: String = ???
    override def context: String = ???
  }

  case class InvalidLoopFormatForIterationContract(loop: Loop[_], message: String) extends UserError {
    override def code: String = "invalidIterationLoop"
    override def text: String =
      loop.o.messageInContext(s"This loop has an iteration contract, but $message.")
  }
}

case class IterationContractToParBlock[Pre <: Generation]() extends Rewriter[Pre] {
  import IterationContractToParBlock._

  def getVariableAndLowerBound(init: Statement[Pre]): Option[(Variable[Pre], Expr[Pre])] =
    init match {
      case Block(Seq(Assign(Local(Ref(v)), low))) =>
        Some((v, low))
      case _ => None
    }

  def getExclusiveUpperBound(v: Variable[Pre], cond: Expr[Pre]): Option[Expr[Pre]] = {
    implicit val o: Origin = IterationContractOrigin
    cond match {
      case Less(Local(Ref(`v`)), high) => Some(high)
      case LessEq(Local(Ref(`v`)), high) => Some(high + const(1))
      case Greater(high, Local(Ref(`v`))) => Some(high)
      case GreaterEq(high, Local(Ref(`v`))) => Some(high + const(1))
      case _ => None
    }
  }

  def assertIncrements(v: Variable[Pre], loop: Loop[Pre]): Unit =
    loop.update match {
      case Block(Seq(Assign(Local(Ref(`v`)), Plus(Local(Ref(`v`)), IntegerValue(ONE))))) =>
      case Block(Seq(Eval(PostAssignExpression(Local(Ref(`v`)), Plus(Local(Ref(`v`)), IntegerValue(ONE)))))) =>
      case _ =>
        throw InvalidLoopFormatForIterationContract(loop,
          "we could not ascertain that the iteration variable is incremented by one each iteration")
    }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case loop @ Loop(init, cond, update, contract @ IterationContract(requires, ensures, context_everywhere), body) =>
      val (v, low) = getVariableAndLowerBound(init).getOrElse(throw InvalidLoopFormatForIterationContract(loop,
        "we could not derive the iteration variable or its lower bound from the initialization portion of the loop"))

      val high = getExclusiveUpperBound(v, cond).getOrElse(throw InvalidLoopFormatForIterationContract(loop,
        "we could not derive an upper bound for the iteration variable from the condition"))

      assertIncrements(v, loop)

      val newV = collectOneInScope(variableScopes) { dispatch(v) }

      implicit val o: Origin = loop.o
      ParStatement(
        ParBlock(
          decl = new ParBlockDecl(),
          iters = Seq(IterVariable(newV, dispatch(low), dispatch(high))),
          requires = dispatch(requires),
          ensures = dispatch(ensures),
          context_everywhere = dispatch(context_everywhere),
          content = dispatch(body),
        )(contract.blame)
      )
    case other => rewriteDefault(other)
  }
}
