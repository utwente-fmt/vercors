package vct.col.ast.lang.c

import vct.col.ast.CEnumSpecifier
import vct.col.ast.ops.CEnumSpecifierOps
import vct.col.print.{Ctx, Doc, Text}

trait CEnumSpecifierImpl[G] extends CEnumSpecifierOps[G] {
  this: CEnumSpecifier[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("enum") <+> name
}
