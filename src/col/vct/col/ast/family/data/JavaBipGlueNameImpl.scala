package vct.col.ast.family.data

import vct.col.ast.JavaBipGlueName
import vct.col.print.{Ctx, Doc, Text}

trait JavaBipGlueNameImpl[G] {
  this: JavaBipGlueName[G] =>
  override def layout(implicit ctx: Ctx): Doc = t.show <> "," <+> e.show
}
