package vct.col.ast.lang

import vct.col.ast.{Expr, SYCLRange, SYCLTRange, Type}
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.util.AstBuildHelpers.ExprBuildHelpers

trait SYCLRangeImpl[G] { this: SYCLRange[G] =>
  val size: Expr[G] = dimensions.reduce((e1, e2) => e1 * e2)

  override def t: Type[G] = SYCLTRange(dimensions.size)

  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::range") <> "<" <> Text(dimensions.size.toString) <> ">" <>
      "(" <> Text(dimensions.mkString(",")) <> ")")
}