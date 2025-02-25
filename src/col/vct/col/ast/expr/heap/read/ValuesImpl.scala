package vct.col.ast.expr.heap.read

import vct.col.ast.{TSeq, Type, Values}
import vct.col.print._
import vct.col.ast.ops.ValuesOps

trait ValuesImpl[G] extends ValuesOps[G] {
  this: Values[G] =>
  override def t: Type[G] = TSeq(arr.t.asArray.get.element)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("\\values(") <> Doc.args(Seq(arr, from, to)) <> ")")
}
