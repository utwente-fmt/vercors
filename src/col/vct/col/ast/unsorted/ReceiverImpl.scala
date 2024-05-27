package vct.col.ast.unsorted

import vct.col.ast.{Receiver, Type}
import vct.col.ast.ops.ReceiverOps
import vct.col.print._

trait ReceiverImpl[G] extends ReceiverOps[G] {
  this: Receiver[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("\\receiver")
  override def t: Type[G] = ref.decl.receiver.get.decl.t
}
