package vct.col.ast.temporaryimplpackage.expr.op.tuple

import vct.col
import vct.col.ast.temporaryimplpackage.expr.ExprImpl
import vct.col.ast.{TTuple, TupGet, Type}
import vct.col.check.{CheckContext, CheckError}

trait TupGetImpl extends ExprImpl { this: TupGet =>
  def tupleType: TTuple = tup.t.asTuple.get
  override def t: Type = tupleType.elements(index)
  override def check(context: CheckContext): Seq[CheckError] =
    super.check(context) match {
      case Nil => if(0 <= index && index < tupleType.elements.size) Nil else Seq(col.check.TypeErrorText(this, _ => "Tuple getter exceeds tuple size"))
      case some => some
    }
}