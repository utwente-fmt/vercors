package vct.col.ast.temporaryimplpackage.declaration.global

import vct.col.ast.GlobalDeclaration
import vct.col.rewrite.ScopeContext

trait GlobalDeclarationImpl { this: GlobalDeclaration =>
  override def declareDefault(scope: ScopeContext): Unit = scope.globalScopes.top += this
}