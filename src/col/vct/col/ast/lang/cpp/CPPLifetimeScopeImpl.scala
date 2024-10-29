package vct.col.ast.lang.cpp

import vct.col.ast.CPPLifetimeScope
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.CPPLifetimeScopeOps

trait CPPLifetimeScopeImpl[G] extends CPPLifetimeScopeOps[G] {
  this: CPPLifetimeScope[G] =>
  override def layout(implicit ctx: Ctx): Doc = body.show
}
