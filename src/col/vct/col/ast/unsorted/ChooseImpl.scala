package vct.col.ast.unsorted

import vct.col.ast.{Choose, Type}
import vct.col.ast.ops.ChooseOps
import vct.col.print._

trait ChooseImpl[G] extends ChooseOps[G] { this: Choose[G] =>
  override lazy val t: Type[G] =
    xs.t.asSet.get.element

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("\\choose(") <> Doc.arg(xs) <> ")")
}
