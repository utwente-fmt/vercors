package vct.col.ast.temporaryimplpackage.declaration.singular

import vct.col.ast.LabelDecl
import vct.col.rewrite.ScopeContext

trait LabelDeclImpl[G] { this: LabelDecl[G] =>
  override def declareDefault[Pre](scope: ScopeContext[Pre, G]): this.type = {
    scope.labelScopes.top += this
    this
  }
}