package vct.col.ast.expr.veymont

import vct.col.ast.ops.SenderOps
import vct.col.ast.{Sender, Type}
import vct.col.print._

trait SenderImpl[G] extends SenderOps[G] {
  this: Sender[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("\\sender")
  override def precedence: Int = Precedence.ATOMIC
  override def t: Type[G] = ref.decl.sender.get.decl.t
}
