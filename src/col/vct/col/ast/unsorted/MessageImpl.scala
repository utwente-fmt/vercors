package vct.col.ast.unsorted

import vct.col.ast.{Message, Type}
import vct.col.ast.ops.MessageOps
import vct.col.print._

trait MessageImpl[G] extends MessageOps[G] { this: Message[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("\\msg")
  override def t: Type[G] = ref.decl.msg.t
}
