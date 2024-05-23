package vct.col.ast.expr.op.collection

import vct.col.ast.{Tail, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.TailOps

trait TailImpl[G] extends TailOps[G] { this: Tail[G] =>
  override def t: Type[G] = xs.t

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = assoc(xs) <> ".tail"
}