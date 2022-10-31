package vct.col.ast.langspecific.cpp

import vct.col.ast.{CPPDeclaration, TResource}
import vct.col.check.{CheckContext, CheckError}
import vct.col.rewrite.ScopeContext

trait CPPDeclarationImpl[G] {
  this: CPPDeclaration[G] =>
  override def declareDefault[Pre](scope: ScopeContext[Pre, G]): this.type = {
    scope.cppLocalScopes.top += this
    this
  }

  override def check(context: CheckContext[G]): Seq[CheckError] = kernelInvariant.checkSubType(TResource())
}
