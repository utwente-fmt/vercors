package vct.col.ast.expr.op.collection

import vct.col.ast.{VectorSubscript, Type}
import vct.col.print.{Ctx, Doc, Precedence, Group}
import vct.col.ast.ops.VectorSubscriptOps

trait VectorSubscriptImpl[G] extends VectorSubscriptOps[G] {
  this: VectorSubscript[G] =>
  override def t: Type[G] = seq.t.asVector.get.element

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(seq) <> "[" <> Doc.arg(index) <> "]")
}
