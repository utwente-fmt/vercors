package vct.col.ast.lang.sycl

import vct.col.ast.SYCLTNDItem
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.SYCLTNDItemOps

trait SYCLTNDItemImpl[G] extends SYCLTNDItemOps[G] { this: SYCLTNDItem[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::nd_item") <> "<" <> Text(dimCount.toString) <> ">")

  override val namespacePath = "sycl::nd_item"
}