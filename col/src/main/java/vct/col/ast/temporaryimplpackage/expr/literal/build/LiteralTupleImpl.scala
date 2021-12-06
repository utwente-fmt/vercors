package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.temporaryimplpackage.expr.ExprImpl
import vct.col.ast.{LiteralTuple, TTuple, Type}
import vct.col.check.{CheckContext, CheckError, TupleTypeCount}

trait LiteralTupleImpl extends ExprImpl { this: LiteralTuple =>
  override def t: Type = TTuple(ts)

  override def check(context: CheckContext): Seq[CheckError] =
    if(ts.size == values.size) {
      super.check(context)
    } else {
      Seq(TupleTypeCount(this))
    }
}