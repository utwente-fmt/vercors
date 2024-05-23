package vct.col.ast.`type`

import vct.col.ast.TRational
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.TRationalOps

trait TRationalImpl[G] extends TRationalOps[G] { this: TRational[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("rational")
}