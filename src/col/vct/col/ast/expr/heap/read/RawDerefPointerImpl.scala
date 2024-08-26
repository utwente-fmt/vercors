package vct.col.ast.expr.heap.read

import vct.col.ast.ops.RawDerefPointerOps
import vct.col.ast.{RawDerefPointer, TRef, Type}
import vct.col.print._

trait RawDerefPointerImpl[G] extends RawDerefPointerOps[G] {
  this: RawDerefPointer[G] =>
  override def t: Type[G] = TRef()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("ptr_deref(") <> pointer <> Text(")"))
}
