package vct.col.ast.`type`

import vct.col.ast.TVeyMontChannel
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.TVeyMontChannelOps

trait TVeyMontChannelImpl[G] extends TVeyMontChannelOps[G] { this: TVeyMontChannel[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(this.channelType)
}