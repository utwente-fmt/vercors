package vct.col.newrewrite

import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import RewriteHelpers._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.rewrite.Rewriter
import vct.result.VerificationResult.UserError

case object IterationContractToParBlock {
  case object IterationContractOrigin extends Origin {
    override def preferredName: String = ???
    override def messageInContext(message: String): String = ???
  }

  case class InvalidLoopFormatForIterationContract(loop: Loop, message: String) extends UserError {
    override def code: String = "invalidIterationLoop"
    override def text: String =
      loop.o.messageInContext(s"This loop has an iteration contract, but $message.")
  }
}

case class IterationContractToParBlock() extends Rewriter {
  import IterationContractToParBlock._

  def getVariableAndLowerBound(init: Statement): Option[(Variable, Expr)] =
    init match {
      case Scope(Seq(v), Block(Seq(Assign(Local(Ref(v1)), low)))) if v == v1 =>
        Some((v, low))
      case _ => None
    }

  def getExclusiveUpperBound(v: Variable, cond: Expr): Option[Expr] = {
    implicit val o: Origin = IterationContractOrigin
    cond match {
      case Less(Local(Ref(`v`)), high) => Some(high)
      case LessEq(Local(Ref(`v`)), high) => Some(high + const(1))
      case Greater(high, Local(Ref(`v`))) => Some(high)
      case GreaterEq(high, Local(Ref(`v`))) => Some(high + const(1))
      case _ => None
    }
  }

  def assertIncrements(v: Variable, loop: Loop): Unit =
    loop.update match {
      case Assign(Local(Ref(`v`)), Plus(Local(Ref(`v`)), Constant(1))) =>
      case _ =>
        throw InvalidLoopFormatForIterationContract(loop,
          "we could not ascertain that the iteration variable is incremented by one each iteration")
    }

  override def dispatch(stat: Statement): Statement = stat match {
    case loop @ Loop(init, cond, update, IterationContract(requires, ensures), body) =>
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
          content = dispatch(body),
        )(???)
      )(???)
    case other => rewriteDefault(other)
  }
}
