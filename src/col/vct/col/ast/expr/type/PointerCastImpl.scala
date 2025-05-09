package vct.col.ast.expr.`type`

import vct.col.ast.ops.PointerCastOps
import vct.col.ast.PointerCast
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait PointerCastImpl[G] extends PointerCastOps[G] {
  this: PointerCast[G] =>

  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("(") <> t <> ")" <> assoc(value)
}
