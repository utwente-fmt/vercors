package vct.col.ast.lang

import vct.col.ast.CInit
import vct.col.print.{Ctx, Doc, Group}

trait CInitImpl[G] { this: CInit[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    if(init.isEmpty) decl.show else Group(decl.show <+> "=" <>> init.get)
}