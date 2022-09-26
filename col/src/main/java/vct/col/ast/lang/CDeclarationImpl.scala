package vct.col.ast.lang

import vct.col.ast.{CDeclaration, TResource}
import vct.col.check.{CheckContext, CheckError}
import vct.col.rewrite.ScopeContext

trait CDeclarationImpl[G] { this: CDeclaration[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] = kernelInvariant.checkSubType(TResource())
}