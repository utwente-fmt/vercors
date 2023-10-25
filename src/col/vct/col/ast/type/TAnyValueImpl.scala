package vct.col.ast.`type`

import vct.col.ast.TAnyValue
import vct.col.print.{Ctx, Doc, Text}

trait TAnyValueImpl[G] { this: TAnyValue[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("any")
}
