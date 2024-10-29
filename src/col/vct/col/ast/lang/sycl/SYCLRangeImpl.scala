package vct.col.ast.lang.sycl

import vct.col.ast.{Expr, SYCLRange, SYCLTRange, Type}
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.util.AstBuildHelpers.ExprBuildHelpers
import vct.col.ast.ops.SYCLRangeOps

trait SYCLRangeImpl[G] extends SYCLRangeOps[G] {
  this: SYCLRange[G] =>
  val size: Expr[G] = dimensions.reduce((e1, e2) => e1 * e2)

  override def t: Type[G] = SYCLTRange(dimensions.size)

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text("sycl::range") <> "<" <> Text(dimensions.size.toString) <> ">" <>
        "(" <> Text(dimensions.mkString(",")) <> ")"
    )
}
