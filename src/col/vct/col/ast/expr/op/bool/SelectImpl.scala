package vct.col.ast.expr.op.bool

import vct.col.ast.{Select, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text, Group}
import vct.col.typerules.Types
import vct.col.ast.ops.SelectOps

trait SelectImpl[G] extends SelectOps[G] {
  this: Select[G] =>
  override lazy val t: Type[G] = Types
    .leastCommonSuperType(whenTrue.t, whenFalse.t)

  override def precedence: Int = Precedence.SELECT
  override def layout(implicit ctx: Ctx): Doc =
    Group(nassoc(condition) <>> { Text("?") <+> nassoc(whenTrue) } <>> {
      Text(":") <+> assoc(whenFalse)
    })
}
