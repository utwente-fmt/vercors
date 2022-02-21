package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{CDeclaration, TResource}
import vct.col.check.{CheckContext, CheckError}
import vct.col.rewrite.ScopeContext

trait CDeclarationImpl[G] { this: CDeclaration[G] =>
  override def declareDefault[Pre](scope: ScopeContext[Pre, G]): this.type = {
    scope.cLocalScopes.top += this
    this
  }

  override def check(context: CheckContext[G]): Seq[CheckError] = kernelInvariant.checkSubType(TResource())
}