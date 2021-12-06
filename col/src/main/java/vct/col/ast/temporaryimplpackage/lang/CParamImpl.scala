package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.CParam
import vct.col.rewrite.ScopeContext

trait CParamImpl { this: CParam =>
  override def declareDefault(scope: ScopeContext): Unit = scope.cParams.top += this
}