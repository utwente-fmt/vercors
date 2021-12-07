package vct.col.newrewrite.exc

import vct.col.ast._
import vct.col.util.AstBuildHelpers._
import RewriteHelpers._
import hre.util.ScopedStack
import vct.col.newrewrite.exc.SwitchToGoto.CaseOutsideSwitch
import vct.col.origin.Origin
import vct.col.rewrite.Rewriter
import vct.result.VerificationResult.UserError

import scala.collection.mutable.ArrayBuffer

case object SwitchToGoto {
  case class CaseOutsideSwitch(c: SwitchCase) extends UserError {
    override def code: String = "case"
    override def text: String =
      c.o.messageInContext("This case occurs outside a switch statement.")
  }
}

case class SwitchToGoto() extends Rewriter {
  val currentCases: ScopedStack[ArrayBuffer[(SwitchCase, LabelDecl)]] = ScopedStack()

  override def dispatch(stat: Statement): Statement = stat match {
    case Switch(expr, body) =>
      implicit val o: Origin = stat.o
      val collectedCases = ArrayBuffer[(SwitchCase, LabelDecl)]()
      val rewrittenBody = currentCases.having(collectedCases) {
        dispatch(body)
      }

      val switchValueVariable = new Variable(dispatch(expr.t))
      val switchValue = switchValueVariable.get

      val normalCaseIfs = Block(collectedCases.collect {
        case (c: Case, label) =>
          Branch(Seq((
            switchValue === c.pattern,
            Goto(label.ref),
          )))
      }.toSeq)

      val (newBody, defaultLabel) = collectedCases.collectFirst {
        case (c: DefaultCase, label) =>
          (rewrittenBody, label)
      }.getOrElse {
        val pastSwitch = new LabelDecl()
        (Block(Seq(rewrittenBody, Label(pastSwitch, Block(Nil)))), pastSwitch)
      }

      Block(Seq(
        Assign(switchValue, dispatch(expr)),
        normalCaseIfs,
        Goto(defaultLabel.ref),
        newBody,
      ))

    case c: SwitchCase =>
      currentCases.topOption match {
        case None => throw CaseOutsideSwitch(c)
        case Some(buf) =>
          implicit val o: Origin = c.o
          val replacementLabel = new LabelDecl()
          buf += ((c, replacementLabel))
          Label(replacementLabel, Block(Nil))
      }

    case other => rewriteDefault(other)
  }
}