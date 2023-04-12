package vct.col.ast.lang

import vct.col.ast.JavaName
import vct.col.print.{Ctx, Doc, Text}

trait JavaNameImpl[G] { this: JavaName[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.fold(names.map(Text))(_ <> "." <> _)
}