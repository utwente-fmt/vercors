package vct.col.ast.lang.c

import vct.col.ast.CEnumDeclaration
import vct.col.ast.ops.CEnumDeclarationOps
import vct.col.print.{Ctx, Doc, Empty, Text}

trait CEnumDeclarationImpl[G] extends CEnumDeclarationOps[G] {
  this: CEnumDeclaration[G] =>
  override def layout(implicit ctx: Ctx): Doc = {
    Doc.stack(Seq(
      Text("enum") <+>
        (if (name.isEmpty)
           Empty
         else
           Text(name.get))
    ))
  }
}
