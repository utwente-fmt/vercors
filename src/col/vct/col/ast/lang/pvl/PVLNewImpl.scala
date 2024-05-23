package vct.col.ast.lang.pvl

import vct.col.ast.PVLNew
import vct.col.print.{Ctx, Doc, DocUtil, Text, Group}
import vct.col.ast.ops.PVLNewOps

trait PVLNewImpl[G] extends PVLNewOps[G] {
  this: PVLNew[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Group(Text("new") <+> t <> "(" <> Doc.args(args) <> ")") <>
        DocUtil.givenYields(givenMap, yields)
    )
}
