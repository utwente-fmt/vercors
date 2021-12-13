package vct.col.newrewrite.exc

import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import RewriteHelpers._
import hre.util.ScopedStack
import vct.col.newrewrite.exc.SwitchToGoto.CaseOutsideSwitch
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationResult.UserError

import scala.collection.mutable.ArrayBuffer

case object SwitchToGoto extends RewriterBuilder {
  case class CaseOutsideSwitch(c: SwitchCase[_]) extends UserError {
    override def code: String = "case"
    override def text: String =
      c.o.messageInContext("This case occurs outside a switch statement.")
  }
}

case class SwitchToGoto[Pre <: Generation]() extends Rewriter[Pre] {
  val currentCases: ScopedStack[ArrayBuffer[(SwitchCase[Pre], LabelDecl[Post])]] = ScopedStack()

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case Switch(expr, body) =>
      implicit val o: Origin = stat.o
      val collectedCases = ArrayBuffer[(SwitchCase[Pre], LabelDecl[Post])]()
      val rewrittenBody = currentCases.having(collectedCases) {
        dispatch(body)
      }

      val switchValueVariable = new Variable[Post](dispatch(expr.t))
      val switchValue = switchValueVariable.get

      val normalCaseIfs = Block(collectedCases.collect {
        case (c: Case[Pre], label) =>
          Branch(Seq((
            switchValue === dispatch(c.pattern),
            Goto[Post](label.ref),
          )))
      }.toSeq)

      val (newBody, defaultLabel) = collectedCases.collectFirst {
        case (c: DefaultCase[Pre], label) =>
          (rewrittenBody, label)
      }.getOrElse {
        val pastSwitch = new LabelDecl[Post]()
        (Block(Seq(rewrittenBody, Label(pastSwitch, Block(Nil)))), pastSwitch)
      }

      Block(Seq(
        Assign(switchValue, dispatch(expr)),
        normalCaseIfs,
        Goto(defaultLabel.ref),
        newBody,
      ))

    case c: SwitchCase[Pre] =>
      currentCases.topOption match {
        case None => throw CaseOutsideSwitch(c)
        case Some(buf) =>
          implicit val o: Origin = c.o
          val replacementLabel = new LabelDecl[Post]()
          buf += ((c, replacementLabel))
          Label(replacementLabel, Block(Nil))
      }

    case other => rewriteDefault(other)
  }
}