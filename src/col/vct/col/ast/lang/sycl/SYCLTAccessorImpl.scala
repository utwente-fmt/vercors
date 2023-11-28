package vct.col.ast.lang.sycl

import vct.col.ast.SYCLTAccessor
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.SYCLTAccessorOps

trait SYCLTAccessorImpl[G] extends SYCLTAccessorOps[G] { this: SYCLTAccessor[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::accessor") <> "<" <> typ <> ", " <> Text(dimCount.toString) <> ">")

  override val namespacePath = "sycl::accessor"
}