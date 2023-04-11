package vct.col.ast.lang

import vct.col.ast.CAnonymousFunctionDeclarator
import vct.col.print._

trait CAnonymousFunctionDeclaratorImpl[G] { this: CAnonymousFunctionDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(inner.show <> "(" <> Doc.args(params.map(Text)) <> ")")
}