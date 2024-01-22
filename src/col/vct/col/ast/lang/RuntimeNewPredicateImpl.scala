package vct.col.ast.lang

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}

trait RuntimeNewPredicateImpl[G] {
  this: RuntimeNewPredicate[G] =>

  override def t: Type[G] = TClass(cls)

  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> ctx.name(cls) <> "(" <> Doc.args(this.args) <> ")"

}