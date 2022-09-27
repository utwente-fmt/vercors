package vct.col.ast.expr.literal.build

import vct.col.ast.expr.ExprImpl
import vct.col.ast.{LiteralTuple, TTuple, Type}
import vct.col.check.{CheckContext, CheckError, TupleTypeCount}

trait LiteralTupleImpl[G] extends ExprImpl[G] { this: LiteralTuple[G] =>
  override def t: Type[G] = TTuple(ts)

  override def check(context: CheckContext[G]): Seq[CheckError] =
    if(ts.size == values.size) {
      super.check(context)
    } else {
      Seq(TupleTypeCount(this))
    }
}