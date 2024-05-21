package vct.col.ast.expr.lock

import vct.col.ast.{Committed, TBool}
import vct.col.print.{Ctx, Doc, Precedence, Text, Group}
import vct.col.ast.ops.CommittedOps

trait CommittedImpl[G] extends CommittedOps[G] { this: Committed[G] =>
  override def t: TBool[G] = TBool()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("committed(") <> Doc.arg(obj) <> ")")
}
