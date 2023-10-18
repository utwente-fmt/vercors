package vct.col.ast.lang

import vct.col.ast.{JavaInterface, Type}
import vct.col.print.{Ctx, Doc, Empty, Group, Show, Text}
import vct.col.util.AstBuildHelpers.tt

trait JavaInterfaceImpl[G] {
  this: JavaInterface[G] =>
  override def supports: Seq[Type[G]] = ext

  override def layout(implicit ctx: Ctx): Doc = {
    Group(
      Doc.spread(modifiers :+ Text("interface")) <+> name <>
        (if (typeParams.isEmpty)
           Empty
         else
           Text("<") <> Doc.args(typeParams) <> ">") <>
        (if (ext.isEmpty)
           Empty
         else
           Empty <+> "extends" <+> Doc.args(ext))
    ) <+> "{" <>> Doc.stack(decls) <+/> "}"
  }
}
