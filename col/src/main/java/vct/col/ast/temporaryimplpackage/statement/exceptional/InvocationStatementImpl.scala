package vct.col.ast.temporaryimplpackage.statement.exceptional

import vct.col.ast.{AbstractMethod, Expr, InvocationStatement, Variable}
import vct.col.ref.Ref

trait InvocationStatementImpl[G] { this: InvocationStatement[G] =>
  override def ref: Ref[G, _ <: AbstractMethod[G]]
  def outArgs: Seq[Ref[G, Variable[G]]]
  def args: Seq[Expr[G]]
}
