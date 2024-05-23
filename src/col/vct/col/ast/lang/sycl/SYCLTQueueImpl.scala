package vct.col.ast.lang.sycl

import vct.col.ast.SYCLTQueue
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.SYCLTQueueOps

trait SYCLTQueueImpl[G] extends SYCLTQueueOps[G] {
  this: SYCLTQueue[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("sycl::queue")

  override val namespacePath = "sycl::queue"
}
