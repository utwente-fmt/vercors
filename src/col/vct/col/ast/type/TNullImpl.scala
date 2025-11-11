package vct.col.ast.`type`

import vct.col.ast.TNull
import vct.col.ast.ops.TNullOps
import vct.col.print.{Ctx, Doc, Text}

trait TNullImpl[G] extends TNullOps[G] {
  this: TNull[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("NullType")
}
