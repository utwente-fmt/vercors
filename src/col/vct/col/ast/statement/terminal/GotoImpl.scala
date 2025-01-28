package vct.col.ast.statement.terminal

import vct.col.ast.{Goto, LabelDecl}
import vct.col.check.{CheckContext, CheckError, OutOfScopeError}
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.GotoOps

trait GotoImpl[G] extends GotoOps[G] {
  this: Goto[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] =
    context.currentApplicable.get.body.get.collectFirst {
      case label: LabelDecl[G] if label == lbl.decl => label
    } match {
      case Some(_) => Seq()
      case None => Seq(OutOfScopeError(this, lbl))
    }

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => Text("goto") <+> ctx.name(lbl)
      case _ => Text("goto") <+> ctx.name(lbl) <> ";"
    }
}
