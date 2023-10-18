package vct.col.ast.lang

import vct.col.ast.SYCLQueue
import vct.col.print.{Ctx, Doc, Text}

trait SYCLQueueImpl[G] {
  this: SYCLQueue[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("sycl::queue")
}
