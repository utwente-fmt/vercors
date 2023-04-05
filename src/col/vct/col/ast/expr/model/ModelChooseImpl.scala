package vct.col.ast.expr.model

import vct.col.ast.{ModelChoose, TVoid, Type}
import vct.col.print.{Ctx, Doc, Precedence, Group}

trait ModelChooseImpl[G] { this: ModelChoose[G] =>
  override def t: Type[G] = TVoid()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(model) <> "." <> "choose" <> "(" <> Doc.args(Seq(perm, totalProcess)) <> ")")
}