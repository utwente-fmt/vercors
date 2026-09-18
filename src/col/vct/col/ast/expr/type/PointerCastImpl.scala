package vct.col.ast.expr.`type`

import vct.col.ast.ops.PointerCastOps
import vct.col.ast.PointerCast
import vct.col.ast.expr.binder.PossibleTriggerImpl
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait PointerCastImpl[G] extends PointerCastOps[G] with PossibleTriggerImpl[G] {
  this: PointerCast[G] =>

  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("(") <> t <> ")" <> assoc(value)

  // Only casts with non-null pointers can be cleanly turned into functions
  override def isPossibleTrigger: Boolean =
    t.asPointer.exists(_.isNonNull) || value.t.asPointer.exists(_.isNonNull)
}
