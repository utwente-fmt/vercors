package vct.col.ast.temporaryimplpackage.declaration.model

import vct.col.ast.ModelDeclaration
import vct.col.rewrite.ScopeContext

trait ModelDeclarationImpl[G] { this: ModelDeclaration[G] =>
  override def declareDefault[Pre](scope: ScopeContext[Pre, G]): Unit = scope.modelScopes.top += this
}
