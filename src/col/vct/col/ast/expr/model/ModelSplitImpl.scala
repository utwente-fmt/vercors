package vct.col.ast.expr.model

import vct.col.ast.{ModelSplit, TVoid, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence}

trait ModelSplitImpl[G] {
  this: ModelSplit[G] =>
  override def t: Type[G] = TVoid()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      assoc(model) <> "." <> "split" <> "(" <>
        Doc.args(Seq(leftPerm, leftProcess, rightPerm, rightProcess)) <> ")"
    )
}
