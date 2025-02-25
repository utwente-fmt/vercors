package vct.col.ast.expr.op.process

import vct.col.ast.{ProcessSelect, TProcess, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.ProcessSelectOps

trait ProcessSelectImpl[G] extends ProcessSelectOps[G] {
  this: ProcessSelect[G] =>
  override def t: Type[G] = TProcess()

  override def precedence: Int = Precedence.SELECT
  override def layout(implicit ctx: Ctx): Doc =
    Group(nassoc(cond) <>> { Text("?") <+> nassoc(whenTrue) } <>> {
      Text(":") <+> assoc(whenFalse)
    })
}
