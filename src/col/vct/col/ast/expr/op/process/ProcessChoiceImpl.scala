package vct.col.ast.expr.op.process

import vct.col.ast.{ProcessChoice, TProcess, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.ProcessChoiceOps

trait ProcessChoiceImpl[G] extends ProcessChoiceOps[G] { this: ProcessChoice[G] =>
  override def t: Type[G] = TProcess()

  override def precedence: Int = Precedence.ADDITIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "+", right)
}