package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait TIntObjectImpl[G] {
  this: TIntObject[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("Integer")
}