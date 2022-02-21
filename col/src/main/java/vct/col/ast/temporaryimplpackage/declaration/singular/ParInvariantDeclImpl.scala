package vct.col.ast.temporaryimplpackage.declaration.singular

import vct.col.ast.ParInvariantDecl
import vct.col.rewrite.ScopeContext

trait ParInvariantDeclImpl[G] { this: ParInvariantDecl[G] =>
  override def declareDefault[Pre](scope: ScopeContext[Pre, G]): this.type = {
    scope.parInvariantScopes.top += this
    this
  }
}