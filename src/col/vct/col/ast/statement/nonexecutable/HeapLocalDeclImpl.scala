package vct.col.ast.statement.nonexecutable

import vct.col.ast.HeapLocalDecl
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.HeapLocalDeclOps

trait HeapLocalDeclImpl[G] extends HeapLocalDeclOps[G] {
  this: HeapLocalDecl[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => Text("var") <+> local.show
      case _ => local.show <> ";"
    }
}
