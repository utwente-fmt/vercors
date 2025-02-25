package vct.col.ast.statement.nonexecutable

import vct.col.ast.{CTArray, LocalDecl}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.LocalDeclOps

trait LocalDeclImpl[G] extends LocalDeclOps[G] {
  this: LocalDecl[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.C =>
        local.t match {
          case a: CTArray[G] =>
            val (spec, decl) = a.innerType.layoutSplitDeclarator
            spec <+> decl <> ctx.name(local) <> Text("[")<> a.size.map(_.show).getOrElse(Text("")) <> Text("]") <> Text(";")
          case _ => local.show <> ";"
        }
      case Ctx.Silver => Text("var") <+> local.show
      case _ => local.show <> ";"
    }
}
