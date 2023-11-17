package vct.col.ast.lang

import vct.col.ast.CStructSpecifier
import vct.col.print.{Ctx, Doc, Group, Text}

trait CStructSpecifierImpl[G] { this: CStructSpecifier[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("struct") <+> name
}