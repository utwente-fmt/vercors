package vct.col.ast.expr.context

import vct.col.ast.{Declaration, ThisDeclaration}
import vct.col.ast.expr.ExprImpl
import vct.col.check.{CheckContext, CheckError, ThisOutsideScopeError}
import vct.col.ref.Ref

trait ThisDeclarationImpl[G] extends ExprImpl[G] { this: ThisDeclaration[G] =>
  def cls: Ref[G, _ <: Declaration[G]]

  override def check(context: CheckContext[G]): Seq[CheckError] =
    if(!context.declarationStack.contains(cls.decl)) super.check(context) ++ Seq(ThisOutsideScopeError(this))
    else super.check(context)
}
