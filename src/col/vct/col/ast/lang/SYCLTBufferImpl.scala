package vct.col.ast.lang

import vct.col.ast.SYCLTBuffer
import vct.col.print.{Ctx, Doc, Group, Text}

trait SYCLTBufferImpl[G] { this: SYCLTBuffer[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::buffer") <> "<" <> typ <> ", " <> Text(dimCount.toString) <> ">")

  override val namespacePath = "sycl::buffer"
}