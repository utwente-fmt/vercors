package vct.col.ast.temporaryimplpackage.declaration.adt

import vct.col.ast.ADTDeclaration
import vct.col.rewrite.ScopeContext

trait ADTDeclarationImpl { this: ADTDeclaration =>
  override def declareDefault(scope: ScopeContext): Unit = scope.adtScopes.top += this
}
