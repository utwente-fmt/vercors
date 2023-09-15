package vct.col.ast.lang

import vct.col.ast.{SYCLNDItem, SYCLTNDItem, Type}
import vct.col.print.{Ctx, Doc, Group, Text}

trait SYCLNDItemImpl[G] { this: SYCLNDItem[G] =>
  override def t: Type[G] = SYCLTNDItem(dimensions.size)

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::nd_item") <> "<" <> Text(dimensions.size.toString) <> ">" <>
      "(" <> Text(dimensions.map(x => x.show).mkString(",")) <> ")")
}