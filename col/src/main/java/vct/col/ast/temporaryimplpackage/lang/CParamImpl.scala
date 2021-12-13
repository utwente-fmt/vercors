package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.CParam
import vct.col.rewrite.ScopeContext

trait CParamImpl[G] { this: CParam[G] =>
  override def declareDefault[Pre](scope: ScopeContext[Pre, G]): Unit = scope.cParams.top += this
}