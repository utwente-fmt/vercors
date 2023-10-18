package vct.col.ast.lang

import vct.col.ast.CPPInit
import vct.col.print.{Ctx, Doc, Group}

trait CPPInitImpl[G] {
  this: CPPInit[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    if (init.isEmpty)
      decl.show
    else
      Group(decl.show <+> "=" <>> init.get)
}
