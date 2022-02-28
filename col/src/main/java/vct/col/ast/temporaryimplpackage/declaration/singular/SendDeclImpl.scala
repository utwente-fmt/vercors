package vct.col.ast.temporaryimplpackage.declaration.singular

import vct.col.ast.SendDecl
import vct.col.rewrite.ScopeContext

trait SendDeclImpl[G] { this: SendDecl[G] =>
  def declareDefault[Pre](scope: ScopeContext[Pre, G]): this.type = {
    scope.sendScopes.top += this
    this
  }
}
