package vct.col.ast.`type`

import vct.col.ast.TVeyMontThread
import vct.col.print._

trait TVeyMontThreadImpl[G] {
  this: TVeyMontThread[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("thread")
}
