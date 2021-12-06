package vct.col.ast.temporaryimplpackage.declaration.singular

import vct.col.ast.ParBlockDecl
import vct.col.rewrite.ScopeContext

trait ParBlockDeclImpl { this: ParBlockDecl =>
  override def declareDefault(scope: ScopeContext): Unit = scope.parBlockScopes.top += this
}