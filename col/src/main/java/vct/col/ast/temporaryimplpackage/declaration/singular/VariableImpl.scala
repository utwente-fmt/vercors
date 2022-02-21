package vct.col.ast.temporaryimplpackage.declaration.singular

import vct.col.ast.Variable
import vct.col.rewrite.ScopeContext

trait VariableImpl[G] { this: Variable[G] =>
  override def declareDefault[Pre](scope: ScopeContext[Pre, G]): this.type = {
    scope.variableScopes.top += this
    this
  }
}