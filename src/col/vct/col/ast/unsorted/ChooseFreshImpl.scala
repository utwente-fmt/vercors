package vct.col.ast.unsorted

import vct.col.ast.{ChooseFresh, Type}
import vct.col.ast.ops.ChooseFreshOps
import vct.col.print._

trait ChooseFreshImpl[G] extends ChooseFreshOps[G] {
  this: ChooseFresh[G] =>
  override lazy val t: Type[G] = xs.t.asSet.get.element

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("\\choose_fresh(") <> Doc.arg(xs) <> ")")
}
