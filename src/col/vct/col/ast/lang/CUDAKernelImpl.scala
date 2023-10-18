package vct.col.ast.lang

import vct.col.ast.CUDAKernel
import vct.col.print.{Ctx, Doc, Text}

trait CUDAKernelImpl[G] {
  this: CUDAKernel[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("__global__")
}
