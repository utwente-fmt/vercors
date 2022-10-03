package vct.col.ast.expr.binder

import vct.col.ast.util.Declarator
import vct.col.ast.{Binder, Declaration, Variable}

trait BinderImpl[G] extends Declarator[G] { this: Binder[G] =>
  def bindings: Seq[Variable[G]]
  override def declarations: Seq[Declaration[G]] = bindings
}