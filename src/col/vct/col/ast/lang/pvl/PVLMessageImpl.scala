package vct.col.ast.lang.pvl

import vct.col.ast.ops.PVLMessageOps
import vct.col.ast.{PVLMessage, Type}
import vct.col.print._

trait PVLMessageImpl[G] extends PVLMessageOps[G] {
  this: PVLMessage[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("/* PVL */ \\msg")
  override def t: Type[G] = ref.get.comm.msg.t
}
