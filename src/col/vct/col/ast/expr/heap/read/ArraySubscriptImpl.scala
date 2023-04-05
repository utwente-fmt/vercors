package vct.col.ast.expr.heap.read

import vct.col.ast.{ArraySubscript, Type}
import vct.col.print._

trait ArraySubscriptImpl[G] { this: ArraySubscript[G] =>
  override lazy val t: Type[G] = arr.t.asArray.get.element

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(arr) <> "[" <>> index </> "]")
}