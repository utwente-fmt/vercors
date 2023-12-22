package vct.col.ast.lang.c

import vct.col.ast.CStructSpecifier
import vct.col.ast.ops.CStructSpecifierOps
import vct.col.print.{Ctx, Doc, Text}

trait CStructSpecifierImpl[G] extends CStructSpecifierOps[G] { this: CStructSpecifier[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("struct") <+> name
}