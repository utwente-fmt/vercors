package vct.col.ast.lang.c

import vct.col.ast.CUDAKernel
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CUDAKernelOps

trait CUDAKernelImpl[G] extends CUDAKernelOps[G] { this: CUDAKernel[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("__global__")
}