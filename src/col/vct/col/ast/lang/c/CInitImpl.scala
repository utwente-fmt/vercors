package vct.col.ast.lang.c

import vct.col.ast.CInit
import vct.col.ast.ops.{CInitFamilyOps, CInitOps}
import vct.col.print.{Ctx, Doc, Group}

trait CInitImpl[G] extends CInitOps[G] with CInitFamilyOps[G] { this: CInit[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    if(init.isEmpty) decl.show else Group(decl.show <+> "=" <>> init.get)
}