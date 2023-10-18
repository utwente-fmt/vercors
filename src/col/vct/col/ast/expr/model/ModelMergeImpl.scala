package vct.col.ast.expr.model

import vct.col.ast.{ModelMerge, TVoid, Type}
import vct.col.print.{Ctx, Doc, Precedence, Group}

trait ModelMergeImpl[G] {
  this: ModelMerge[G] =>
  override def t: Type[G] = TVoid()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      assoc(model) <> "." <> "merge" <> "(" <>
        Doc.args(Seq(leftPerm, leftProcess, rightPerm, rightProcess)) <> ")"
    )
}
