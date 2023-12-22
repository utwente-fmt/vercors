package vct.col.ast.lang.cpp

import vct.col.ast.CPPParam
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.{CPPParamOps, CPPParamFamilyOps}

trait CPPParamImpl[G] extends CPPParamOps[G] with CPPParamFamilyOps[G] { this: CPPParam[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.spread(specifiers) <+> declarator
}