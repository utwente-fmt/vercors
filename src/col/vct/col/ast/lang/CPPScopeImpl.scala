package vct.col.ast.lang

import vct.col.ast.CPPScope
import vct.col.print._

trait CPPScopeImpl[G] {this: CPPScope[G] =>

  override def layout(implicit ctx: Ctx): Doc = layoutAsBlock
  override def foldBlock(f: (Doc, Doc) => Doc)(implicit ctx: Ctx): Doc =
    NodeDoc(this,
      Doc.fold(locals.map(local => ctx.syntax match {
        case Ctx.Silver => Text("var") <+> local
        case _ => local.show <> ";"
      }) :+ body.foldBlock(f))(f)
    )
}