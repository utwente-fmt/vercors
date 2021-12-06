package vct.col.ast.temporaryimplpackage.declaration.singular

import vct.col.ast.Variable
import vct.col.rewrite.ScopeContext

trait VariableImpl { this: Variable =>
  override def declareDefault(scope: ScopeContext): Unit = scope.variableScopes.top += this
}