package vct.col.ast.lang

import vct.col.ast.PVLNew
import vct.col.print.{Ctx, Doc, DocUtil, Text, Group}

trait PVLNewImpl[G] {
  this: PVLNew[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Group(Text("new") <+> t <> "(" <> Doc.args(args) <> ")") <>
        DocUtil.givenYields(givenMap, yields)
    )
}
