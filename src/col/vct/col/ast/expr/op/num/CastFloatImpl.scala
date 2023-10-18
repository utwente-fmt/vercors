package vct.col.ast.expr.op.num

import vct.col.ast.`type`.TFloats
import vct.col.ast.{CastFloat, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait CastFloatImpl[G] {
  this: CastFloat[G] =>
  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("(") <> t <> ")" <> assoc(e)
}
