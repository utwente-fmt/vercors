package vct.col.ast.lang

import vct.col.ast.CPPLifetimeScope
import vct.col.print.{Ctx, Doc}

trait CPPLifetimeScopeImpl[G] {this: CPPLifetimeScope[G] =>
  override def layout(implicit ctx: Ctx): Doc = body.show
}