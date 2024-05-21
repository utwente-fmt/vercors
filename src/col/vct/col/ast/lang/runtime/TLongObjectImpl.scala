package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.TLongObjectOps

trait TLongObjectImpl[G] extends TLongObjectOps[G] {
  this: TLongObject[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("Long")
}