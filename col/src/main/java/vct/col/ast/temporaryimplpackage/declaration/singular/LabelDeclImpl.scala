package vct.col.ast.temporaryimplpackage.declaration.singular

import vct.col.ast.LabelDecl
import vct.col.rewrite.ScopeContext

trait LabelDeclImpl[G] { this: LabelDecl[G] =>
  override def declareDefault[Pre](scope: ScopeContext[Pre, G]): Unit = scope.labelScopes.top += this
}