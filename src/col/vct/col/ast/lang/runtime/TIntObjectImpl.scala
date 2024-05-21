package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.TIntObjectOps

trait TIntObjectImpl[G] extends TIntObjectOps[G] {
  this: TIntObject[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("Integer")
}