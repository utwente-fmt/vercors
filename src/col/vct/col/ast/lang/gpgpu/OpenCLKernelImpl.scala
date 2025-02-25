package vct.col.ast.lang.gpgpu

import vct.col.ast.OpenCLKernel
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.OpenCLKernelOps

trait OpenCLKernelImpl[G] extends OpenCLKernelOps[G] {
  this: OpenCLKernel[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("__kernel")
}
