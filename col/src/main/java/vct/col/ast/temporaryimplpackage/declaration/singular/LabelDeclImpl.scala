package vct.col.ast.temporaryimplpackage.declaration.singular

import vct.col.ast.LabelDecl
import vct.col.rewrite.ScopeContext

trait LabelDeclImpl { this: LabelDecl =>
  override def declareDefault(scope: ScopeContext): Unit = scope.labelScopes.top += this
}