package vct.col.ast.lang

import vct.col.ast.SYCLTItem
import vct.col.print.{Ctx, Doc, Group, Text}

trait SYCLTItemImpl[G] { this: SYCLTItem[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::item") <> "<" <> Text(dimCount.toString) <> ">")

  override val namespacePath = "sycl::item"
}