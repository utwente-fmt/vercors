package vct.col.ast.expr.op.collection

import vct.col.ast.{Take, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.TakeOps

trait TakeImpl[G] extends TakeOps[G] { this: Take[G] =>
  override def t: Type[G] = xs.t

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = assoc(xs) <> "[.." <> count <> "]"
}