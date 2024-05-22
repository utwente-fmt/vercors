package vct.col.ast.statement.composite

import vct.col.ast.Scope
import vct.col.check.CheckContext
import vct.col.print._
import vct.col.resolve.ResolveReferences
import vct.col.ast.ops.ScopeOps

trait ScopeImpl[G] extends ScopeOps[G] {
  this: Scope[G] =>

  override def enterCheckContextScopes(context: CheckContext[G]): Seq[CheckContext.ScopeFrame[G]] =
    context.withScope(locals, toScan = Seq(body))

  override def layout(implicit ctx: Ctx): Doc = layoutAsBlock
  override def foldBlock(f: (Doc, Doc) => Doc)(implicit ctx: Ctx): Doc =
    NodeDoc(this,
      Doc.fold(locals.map(local => ctx.syntax match {
        case Ctx.Silver => Text("var") <+> local
        case _ => local.show <> ";"
      }) :+ body.foldBlock(f))(f)
    )
}