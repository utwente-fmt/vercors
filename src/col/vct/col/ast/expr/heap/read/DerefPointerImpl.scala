package vct.col.ast.expr.heap.read

import vct.col.ast.{DerefPointer, Type}
import vct.col.print._
import vct.col.ast.ops.DerefPointerOps

trait DerefPointerImpl[G] extends DerefPointerOps[G] { this: DerefPointer[G] =>
  override def t: Type[G] = pointer.t.asPointer.get.element

  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("*") <> assoc(pointer)
}