package vct.col.ast.temporaryimplpackage.declaration.singular

import vct.col.ast.ParInvariantDecl
import vct.col.rewrite.ScopeContext

trait ParInvariantDeclImpl { this: ParInvariantDecl =>
  override def declareDefault(scope: ScopeContext): Unit = scope.parInvariantScopes.top += this
}