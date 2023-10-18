package vct.col.ast.expr.heap.read

import vct.col.ast.{PointerSubscript, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence}

trait PointerSubscriptImpl[G] {
  this: PointerSubscript[G] =>
  override def t: Type[G] = pointer.t.asPointer.get.element

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(pointer) <> "[" <> Doc.arg(index) <> "]")
}
