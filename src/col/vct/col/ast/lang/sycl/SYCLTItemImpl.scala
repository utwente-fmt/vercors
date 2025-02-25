package vct.col.ast.lang.sycl

import vct.col.ast.SYCLTItem
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.SYCLTItemOps

trait SYCLTItemImpl[G] extends SYCLTItemOps[G] {
  this: SYCLTItem[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::item") <> "<" <> Text(dimCount.toString) <> ">")

  override val namespacePath = "sycl::item"
}
