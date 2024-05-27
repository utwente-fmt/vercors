package vct.col.ast.lang.c

import vct.col.ast.CAnonymousFunctionDeclarator
import vct.col.ast.ops.CAnonymousFunctionDeclaratorOps
import vct.col.print._

trait CAnonymousFunctionDeclaratorImpl[G]
    extends CAnonymousFunctionDeclaratorOps[G] {
  this: CAnonymousFunctionDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(inner.show <> "(" <> Doc.args(params.map(Text)) <> ")")
}
