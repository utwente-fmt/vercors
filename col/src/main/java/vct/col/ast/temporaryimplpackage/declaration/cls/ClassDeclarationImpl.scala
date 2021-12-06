package vct.col.ast.temporaryimplpackage.declaration.cls

import vct.col.ast.ClassDeclaration
import vct.col.rewrite.ScopeContext

trait ClassDeclarationImpl { this: ClassDeclaration =>
  override def declareDefault(scope: ScopeContext): Unit = scope.classScopes.top += this
}