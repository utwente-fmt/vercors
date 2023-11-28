package vct.col.ast.lang.sycl

import vct.col.ast.SYCLTBuffer
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.SYCLTBufferOps

trait SYCLTBufferImpl[G] extends SYCLTBufferOps[G] { this: SYCLTBuffer[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::buffer") <> "<" <> typ <> ", " <> Text(dimCount.toString) <> ">")

  override val namespacePath = "sycl::buffer"
}