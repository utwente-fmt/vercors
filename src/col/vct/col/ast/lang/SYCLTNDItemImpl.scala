package vct.col.ast.lang

import vct.col.ast.SYCLTNDItem
import vct.col.print.{Ctx, Doc, Group, Text}

trait SYCLTNDItemImpl[G] { this: SYCLTNDItem[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::nd_item") <> "<" <> Text(dimCount.toString) <> ">")
}