package vct.col.ast.expr.literal.build

import vct.col.ast.expr.ExprImpl
import vct.col.ast.{LiteralTuple, TTuple, Type}
import vct.col.check.{CheckContext, CheckMessage, TupleTypeCount}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait LiteralTupleImpl[G] extends ExprImpl[G] { this: LiteralTuple[G] =>
  override def t: Type[G] = TTuple(ts)

  override def check(context: CheckContext[G]): Seq[CheckMessage] =
    if(ts.size == values.size) {
      super.check(context)
    } else {
      Seq(TupleTypeCount(this))
    }

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("tuple<") <> Doc.args(ts) <> ">{" <> Doc.args(values) <> "}")
}