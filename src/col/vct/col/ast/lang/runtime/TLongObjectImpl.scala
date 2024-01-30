package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait TLongObjectImpl[G] {
  this: TLongObject[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("Long")
}