package vct.col.ast.lang

import vct.col.ast.{SYCLItem, SYCLTItem, Type}
import vct.col.print.{Ctx, Doc, Group, Text}

trait SYCLItemImpl[G] { this: SYCLItem[G] =>
  override def t: Type[G] = SYCLTItem(dimensions.size)

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::item") <> "<" <> Text(dimensions.size.toString) <> ">" <>
      "(" <> Text(dimensions.mkString(",")) <> ")")
}