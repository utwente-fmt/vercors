package vct.col.ast.`type`

import vct.col.ast.TArray
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.TArrayOps

trait TArrayImpl[G] extends TArrayOps[G] { this: TArray[G] =>
  override def layout(implicit ctx: Ctx): Doc = element.show <> "[]"
}