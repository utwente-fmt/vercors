package vct.col.ast.lang.java

import vct.col.ast.{JavaFields, JavaStatic}
import vct.col.print._
import vct.col.ast.ops.JavaFieldsOps

trait JavaFieldsImpl[G] extends JavaFieldsOps[G] { this: JavaFields[G] =>
  override def isStatic = modifiers.contains(JavaStatic[G]())

  override def layout(implicit ctx: Ctx): Doc =
    Doc.rspread(modifiers) <> t <+> Doc.spread(decls) <> ";"
}