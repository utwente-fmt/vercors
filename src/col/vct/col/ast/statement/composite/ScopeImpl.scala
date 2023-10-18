package vct.col.ast.statement.composite

import vct.col.ast.Scope
import vct.col.check.CheckContext
import vct.col.print._
import vct.col.resolve.ResolveReferences

trait ScopeImpl[G] {
  this: Scope[G] =>
  override def enterCheckContext(context: CheckContext[G]): CheckContext[G] =
    context.withScope(locals, toScan = Seq(body))

  override def layout(implicit ctx: Ctx): Doc = layoutAsBlock
  override def blockElementsForLayout(implicit ctx: Ctx): Seq[Show] =
    locals.map(local =>
      ctx.syntax match {
        case Ctx.Silver => Text("var") <+> local
        case _ => local.show <> ";"
      }
    ) ++ body.blockElementsForLayout
}
