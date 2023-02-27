package vct.col.ast.statement.exceptional

import vct.col.ast.{AbstractMethod, Expr, InvocationStatement, Variable}
import vct.col.ref.Ref

trait InvocationStatementImpl[G] { this: InvocationStatement[G] =>
  override def ref: Ref[G, _ <: AbstractMethod[G]]
  def outArgs: Seq[Expr[G]]
  def args: Seq[Expr[G]]
}
