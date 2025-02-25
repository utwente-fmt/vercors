package vct.col.ast.expr.model

import vct.col.ast.{ModelAbstractState, TResource, Type}
import vct.col.print._
import vct.col.ast.ops.ModelAbstractStateOps

trait ModelAbstractStateImpl[G] extends ModelAbstractStateOps[G] {
  this: ModelAbstractState[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      assoc(model) <> "." <> "abstractState" <> "(" <> Doc.args(Seq(state)) <>
        ")"
    )
}
