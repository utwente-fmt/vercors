package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.CLocalDeclaration
import vct.col.rewrite.ScopeContext

trait CLocalDeclarationImpl[G] { this: CLocalDeclaration[G] =>
  override def declareDefault[Pre](scope: ScopeContext[Pre, G]): this.type = {
    scope.cLocalScopes.top += this
    this
  }
}
