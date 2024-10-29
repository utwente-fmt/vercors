package vct.col.ast.expr.literal.constant

import vct.col.ast.{TBoundedInt, Type, WritePerm}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.WritePermOps

trait WritePermImpl[G] extends WritePermOps[G] {
  this: WritePerm[G] =>
  override def t: Type[G] = TBoundedInt(1, 2)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("write")
}
