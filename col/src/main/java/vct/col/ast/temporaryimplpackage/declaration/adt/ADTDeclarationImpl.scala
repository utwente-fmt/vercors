package vct.col.ast.temporaryimplpackage.declaration.adt

import vct.col.ast.ADTDeclaration
import vct.col.rewrite.ScopeContext

trait ADTDeclarationImpl[G] { this: ADTDeclaration[G] =>
  override def declareDefault[Pre](scope: ScopeContext[Pre, G]): Unit = scope.adtScopes.top += this
}
