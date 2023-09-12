package vct.col.ast.lang

import vct.col.ast.{SYCLQueue, SYCLTQueue, Type}
import vct.col.print.{Ctx, Doc, Text}

trait SYCLQueueImpl[G] { this: SYCLQueue[G] =>
  override def t: Type[G] = SYCLTQueue()

  override def layout(implicit ctx: Ctx): Doc = Text("sycl::queue()")
}