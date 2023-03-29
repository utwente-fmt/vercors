package vct.col.ast.expr.misc

import vct.col.ast.{ApplyCoercion, Type}

trait ApplyCoercionImpl[G] { this: ApplyCoercion[G] =>
  override def t: Type[G] = coercion.target
}
