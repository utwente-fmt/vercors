package vct.col.ast.family.data

import vct.col.ast.JavaBipGlueName
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.{JavaBipGlueNameOps, JavaBipGlueNameFamilyOps}

trait JavaBipGlueNameImpl[G] extends JavaBipGlueNameOps[G] with JavaBipGlueNameFamilyOps[G] { this: JavaBipGlueName[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    t.show <> "," <+> e.show
}
