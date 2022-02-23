package vct.col.ast.temporaryimplpackage.declaration.cls

import vct.col.ast.ClassDeclaration
import vct.col.rewrite.ScopeContext

trait ClassDeclarationImpl[G] { this: ClassDeclaration[G] =>
  override def declareDefault[Pre](scope: ScopeContext[Pre, G]): this.type = {
    scope.classScopes.top += this
    this
  }
}