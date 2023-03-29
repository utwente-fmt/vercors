package vct.col.ast.statement.terminal

import vct.col.ast.{Goto, LabelDecl}
import vct.col.check.{CheckContext, CheckError, OutOfScopeError}

trait GotoImpl[G] { this: Goto[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] =
    context.currentApplicable.get.body.get.transSubnodes.collectFirst {
      case label: LabelDecl[G] if label == lbl.decl => label
    } match {
      case Some(_) => Seq()
      case None => Seq(OutOfScopeError(this, lbl))
    }
}