package vct.col.rewrite.exc

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.{Origin, TrueSatisfiable}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers.tt
import vct.result.VerificationError.UserError

case object SpecifyImplicitLabels extends RewriterBuilder {
  override def key: String = "implicitLabels"
  override def desc: String =
    "Give loops and switches a label if it needs one for a break or continue statement."

  private case class UnexpectedControlFlow(node: Statement[_])
      extends UserError {
    override def code: String = "unexpectedControlFlow"

    override def text: String =
      node.o.messageInContext(
        "This statement is only allowed inside of a loop and switch statements"
      )
  }

  private def ImplicitLabelOrigin(inner: Origin): Origin =
    inner.where(name = "loop")
}

case class SpecifyImplicitLabels[Pre <: Generation]() extends Rewriter[Pre] {
  import SpecifyImplicitLabels._

  val labelStack = new ScopedStack[LabelDecl[Post]]()

  private def isBreakable(s: Statement[_]): Boolean =
    s match {
      case _: Loop[_] => true
      case _: Switch[_] => true
      case _ => false
    }

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case Label(decl, impl, contract) if isBreakable(impl) =>
        val newLabel = decl.rewrite()
        labelDecls.succeedOnly(decl, newLabel)
        val newImpl = labelStack.having(newLabel) { impl.rewriteDefault() }
        Label(newLabel, newImpl, dispatch(contract))(stat.o)
      case stat if isBreakable(stat) =>
        implicit val o: Origin = stat.o
        val labelDecl = new LabelDecl[Post]()(ImplicitLabelOrigin(o))
        labelStack.having(labelDecl) {
          Label(
            labelDecl,
            stat.rewriteDefault(),
            LoopInvariant(tt, None)(TrueSatisfiable),
          )
        }
      case Continue(None) if labelStack.isEmpty =>
        throw UnexpectedControlFlow(stat)
      case c @ Continue(None) => c.rewrite(Some(labelStack.top.ref))
      case Break(None) if labelStack.isEmpty =>
        throw UnexpectedControlFlow(stat)
      case b @ Break(None) => b.rewrite(Some(labelStack.top.ref))
      case other => super.dispatch(other)
    }
}
