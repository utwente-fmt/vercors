package vct.col.ast.unsorted

import vct.col.ast.{PVLMessage, Type}
import vct.col.ast.ops.PVLMessageOps
import vct.col.print._

trait PVLMessageImpl[G] extends PVLMessageOps[G] {
  this: PVLMessage[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("/* PVL */ \\msg")
  override def t: Type[G] = ref.get.msg.t
}
