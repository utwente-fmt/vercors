package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{Applicable, Apply, Expr, Type}
import vct.col.ref.Ref

trait ApplyImpl[G] { this: Apply[G] =>
  def ref: Ref[G, _ <: Applicable[G]]
  def args: Seq[Expr[G]]

  override def t: Type[G] = ref.decl.returnType
}