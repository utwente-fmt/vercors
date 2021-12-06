package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.JavaLocalDeclaration
import vct.col.rewrite.ScopeContext

trait JavaLocalDeclarationImpl { this: JavaLocalDeclaration =>
  override def declareDefault(scope: ScopeContext): Unit = scope.javaLocalScopes.top += this
}