package vct.col.ast.expr.heap.read

import vct.col.ast.{PointerAdd, Type}
import vct.col.print._
import vct.col.ast.ops.PointerAddOps

trait PointerAddImpl[G] extends PointerAddOps[G] { this: PointerAdd[G] =>
  override def t: Type[G] = pointer.t

  override def precedence: Int = Precedence.ADDITIVE
  override def layout(implicit ctx: Ctx): Doc =
    lassoc(pointer, "+", offset)
}