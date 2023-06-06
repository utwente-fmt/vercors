package vct.col.ast.lang

import vct.col.ast.OpenCLKernel
import vct.col.print.{Ctx, Doc, Text}

trait OpenCLKernelImpl[G] { this: OpenCLKernel[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("__kernel")
}