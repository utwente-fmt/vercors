package vct.col.ast.lang

import vct.col.ast.{JavaFields, JavaStatic}
import vct.col.print._

trait JavaFieldsImpl[G] { this: JavaFields[G] =>
  override def isStatic = modifiers.contains(JavaStatic[G]())

  override def layout(implicit ctx: Ctx): Doc =
    Doc.rspread(modifiers) <> t <+> Doc.spread(decls) <> ";"
}