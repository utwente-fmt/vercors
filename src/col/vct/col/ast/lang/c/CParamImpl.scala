package vct.col.ast.lang.c

import vct.col.ast.CParam
import vct.col.ast.ops.{CParamFamilyOps, CParamOps}
import vct.col.print.{Ctx, Doc}

trait CParamImpl[G] extends CParamOps[G] with CParamFamilyOps[G] { this: CParam[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.spread(specifiers) <+> declarator
}