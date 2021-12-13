package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.JavaLocalDeclaration
import vct.col.rewrite.ScopeContext

trait JavaLocalDeclarationImpl[G] { this: JavaLocalDeclaration[G] =>
  override def declareDefault[Pre](scope: ScopeContext[Pre, G]): Unit = scope.javaLocalScopes.top += this
}