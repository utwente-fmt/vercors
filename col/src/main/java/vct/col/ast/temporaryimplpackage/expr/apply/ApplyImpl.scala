package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{Applicable, Apply, Expr, Type}
import vct.col.ref.Ref

trait ApplyImpl { this: Apply =>
  def ref: Ref[_ <: Applicable]
  def args: Seq[Expr]

  override def t: Type = ref.decl.returnType
}