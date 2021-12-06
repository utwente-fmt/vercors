package vct.col.ast.temporaryimplpackage.declaration.model

import vct.col.ast.ModelDeclaration
import vct.col.rewrite.ScopeContext

trait ModelDeclarationImpl { this: ModelDeclaration =>
  override def declareDefault(scope: ScopeContext): Unit = scope.modelScopes.top += this
}
