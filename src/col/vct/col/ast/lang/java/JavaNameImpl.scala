package vct.col.ast.lang.java

import vct.col.ast.JavaName
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.{JavaNameOps, JavaNameFamilyOps}

trait JavaNameImpl[G] extends JavaNameOps[G] with JavaNameFamilyOps[G] { this: JavaName[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.fold(names.map(Text))(_ <> "." <> _)
}