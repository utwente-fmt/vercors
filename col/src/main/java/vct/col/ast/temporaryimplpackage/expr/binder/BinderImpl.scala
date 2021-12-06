package vct.col.ast.temporaryimplpackage.expr.binder

import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.ast.{Binder, Declaration, Variable}

trait BinderImpl extends Declarator { this: Binder =>
  def bindings: Seq[Variable]
  override def declarations: Seq[Declaration] = bindings
}