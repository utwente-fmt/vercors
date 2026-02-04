package vct.col.ast.expr.`type`

import vct.col.ast.{TNonNullImmutablePointer, TNonNullPointer, ToNonNull, Type}
import vct.col.ast.ops.ToNonNullOps
import vct.col.print.{Ctx, Doc, Text}

trait ToNonNullImpl[G] extends ToNonNullOps[G] {
  this: ToNonNull[G] =>

  override def t: Type[G] = {
    val ptr = value.t.asPointer.get
    if (ptr.isImmutable)
      TNonNullImmutablePointer(ptr.element)
    else
      TNonNullPointer(ptr.element, ptr.unique)
  }

  override def layout(implicit ctx: Ctx): Doc =
    Text("toNonNull(") <> value <> ")"
}
