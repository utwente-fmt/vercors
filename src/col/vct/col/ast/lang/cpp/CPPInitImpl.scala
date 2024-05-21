package vct.col.ast.lang.cpp

import vct.col.ast.CPPInit
import vct.col.print.{Ctx, Doc, Group}
import vct.col.ast.ops.{CPPInitOps, CPPInitFamilyOps}

trait CPPInitImpl[G] extends CPPInitOps[G] with CPPInitFamilyOps[G] { this: CPPInit[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    if(init.isEmpty) decl.show else Group(decl.show <+> "=" <>> init.get)
}