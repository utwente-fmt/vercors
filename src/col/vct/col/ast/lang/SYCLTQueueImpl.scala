package vct.col.ast.lang

import vct.col.ast.SYCLTQueue
import vct.col.print.{Ctx, Doc, Text}

trait SYCLTQueueImpl[G] { this: SYCLTQueue[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("sycl::queue")
}