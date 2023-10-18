package vct.col.ast.`type`

import vct.col.ast.TSYCLQueue
import vct.col.print.{Ctx, Doc, Text}

trait TSYCLQueueImpl[G] {
  this: TSYCLQueue[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("sycl::queue")
}
