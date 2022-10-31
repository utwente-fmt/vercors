package vct.col.ast.langspecific.cpp

import vct.col.ast.CPPParam
import vct.col.rewrite.ScopeContext

trait CPPParamImpl[G] {
  this: CPPParam[G] =>
  override def declareDefault[Pre](scope: ScopeContext[Pre, G]): this.type = {
    scope.cppParams.top += this
    this
  }}
