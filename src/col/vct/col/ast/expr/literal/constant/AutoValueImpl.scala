package vct.col.ast.expr.literal.constant

import vct.col.ast.ops.AutoValueOps
import vct.col.ast.{AutoValue, TResource, Type}
import vct.col.print._

trait AutoValueImpl[G] extends AutoValueOps[G] {
  this: AutoValue[G] =>
  override def t: Type[G] = TResource()

  override def layout(implicit ctx: Ctx): Doc = Text("AutoValue(") <> loc <> ")"
}
