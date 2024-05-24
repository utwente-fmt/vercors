package vct.col.ast.statement.nonexecutable

import vct.col.ast.LocalDecl
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.LocalDeclOps

trait LocalDeclImpl[G] extends LocalDeclOps[G] { this: LocalDecl[G] =>
  override def layout(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.Silver => Text("var") <+> local.show
    case _ => local.show <> ";"
  }
}