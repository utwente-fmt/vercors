package vct.col.ast.expr.veymont

import vct.col.ast.ops.MessageOps
import vct.col.ast.{Message, Type}
import vct.col.print._

trait MessageImpl[G] extends MessageOps[G] {
  this: Message[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("\\msg")
  override def precedence: Int = Precedence.ATOMIC
  override def t: Type[G] = ref.decl.msg.t
}
