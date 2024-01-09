package vct.col.ast.expr.literal.build

import vct.col.ast.expr.ExprImpl
import vct.col.ast.{LiteralTuple, TTuple, Type}
import vct.col.check.{CheckContext, CheckError, TupleTypeCount}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.LiteralTupleOps

trait LiteralTupleImpl[G] extends ExprImpl[G] with LiteralTupleOps[G] { this: LiteralTuple[G] =>
  override def t: Type[G] = TTuple(ts)

  override def check(context: CheckContext[G]): Seq[CheckError] =
    if(ts.size == values.size) {
      super.check(context)
    } else {
      Seq(TupleTypeCount(this))
    }

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("tuple<") <> Doc.args(ts) <> ">{" <> Doc.args(values) <> "}")
}