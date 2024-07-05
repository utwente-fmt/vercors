package vct.col.ast.unsorted

import vct.col.ast.{PVLSender, Type}
import vct.col.ast.ops.PVLSenderOps
import vct.col.print._

trait PVLSenderImpl[G] extends PVLSenderOps[G] {
  this: PVLSender[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("/* PVL */ \\sender")
  override def t: Type[G] = ref.get.comm.inferredSender.get.t
}
