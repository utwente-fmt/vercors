package vct.col.ast.lang

import vct.col.ast.{SilverNull, TRef}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait SilverNullImpl[G] { this: SilverNull[G] =>
  override def t: TRef[G] = TRef[G]()

  override def layout(implicit ctx: Ctx): Doc = Text("null")
}
