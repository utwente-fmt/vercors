package vct.col.ast.expr.binder

import vct.col.ast.{ForPermWithValue, TBool, Variable}

trait ForPermWithValueImpl[G] { this: ForPermWithValue[G] =>
  override def bindings: Seq[Variable[G]] = Seq(binding)
  override def t: TBool[G] = TBool()
}
