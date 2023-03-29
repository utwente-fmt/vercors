package vct.col.ast.expr.apply

import vct.col.ast.{ClassDeclaration, Expr, InstanceApply}
import vct.col.ref.Ref

trait InstanceApplyImpl[G] { this: InstanceApply[G] =>
  def ref: Ref[G, _ <: ClassDeclaration[G]]
  def obj: Expr[G]
}
