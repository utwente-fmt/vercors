package vct.col.ast.`type`

import vct.col.ast.TInt
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.TIntOps

trait TIntImpl[G] extends TIntOps[G] { this: TInt[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("int")
}