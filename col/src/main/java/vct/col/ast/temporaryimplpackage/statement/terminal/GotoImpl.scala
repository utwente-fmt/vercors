package vct.col.ast.temporaryimplpackage.statement.terminal

import vct.col.ast.{Goto, LabelDecl}
import vct.col.check.{CheckContext, CheckError, OutOfScopeError}

trait GotoImpl { this: Goto =>
  override def check(context: CheckContext): Seq[CheckError] =
    context.currentApplicable.get.body.get.transSubnodes.collectFirst {
      case label: LabelDecl if label == lbl.decl => label
    } match {
      case Some(_) => Seq()
      case None => Seq(OutOfScopeError(this, lbl))
    }
}