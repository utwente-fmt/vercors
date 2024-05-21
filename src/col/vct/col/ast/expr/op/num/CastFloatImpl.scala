package vct.col.ast.expr.op.num

import vct.col.ast.{CastFloat}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.CastFloatOps

trait CastFloatImpl[G] extends CastFloatOps[G] { this: CastFloat[G] =>
  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("(") <> t <> ")" <> assoc(e)
}
