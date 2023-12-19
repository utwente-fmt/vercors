package vct.col.ast.expr.op.process

import vct.col.ast.{ProcessPar, TProcess, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.ProcessParOps

trait ProcessParImpl[G] extends ProcessParOps[G] { this: ProcessPar[G] =>
  override def t: Type[G] = TProcess()

  override def precedence: Int = Precedence.OR
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "||", right)
}