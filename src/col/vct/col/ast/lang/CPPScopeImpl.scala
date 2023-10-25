package vct.col.ast.lang

import vct.col.ast.CPPScope
import vct.col.print._

trait CPPScopeImpl[G] {this: CPPScope[G] =>

  override def layout(implicit ctx: Ctx): Doc = layoutAsBlock
  override def blockElementsForLayout(implicit ctx: Ctx): Seq[Show] =
    locals.map(local => ctx.syntax match {
      case Ctx.Silver => Text("var") <+> local
      case _ => local.show <> ";"
    }) ++ body.blockElementsForLayout
}