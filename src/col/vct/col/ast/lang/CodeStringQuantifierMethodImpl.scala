package vct.col.ast.lang

import vct.col.ast.{CodeString, CodeStringQuantifierMethod}
import vct.col.print.{Ctx, Doc, Group, Text}

trait CodeStringQuantifierMethodImpl[G] {
  this: CodeStringQuantifierMethod[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Group(Text("public boolean __runtime__")))

}