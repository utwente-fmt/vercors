package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.RuntimeNewPredicateOps

trait RuntimeNewPredicateImpl[G] extends RuntimeNewPredicateOps[G] {
  this: RuntimeNewPredicate[G] =>

  override def t: Type[G] = TClass(cls, Nil)

  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> ctx.name(cls) <> "(" <> Doc.args(this.args) <> ")"

}