package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{CDeclaration, TResource}
import vct.col.check.{CheckContext, CheckError}
import vct.col.rewrite.ScopeContext

trait CDeclarationImpl { this: CDeclaration =>
  override def declareDefault(scope: ScopeContext): Unit = scope.cLocalScopes.top += this
  override def check(context: CheckContext): Seq[CheckError] = kernelInvariant.checkSubType(TResource())
}