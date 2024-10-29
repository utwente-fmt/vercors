package vct.col.ast.expr.misc

import vct.col.ast.{ApplyCoercion, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.ApplyCoercionOps

trait ApplyCoercionImpl[G] extends ApplyCoercionOps[G] {
  this: ApplyCoercion[G] =>
  override def t: Type[G] = coercion.target
}
