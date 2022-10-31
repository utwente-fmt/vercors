package vct.col.ast.expr.op.tuple

import vct.col
import vct.col.ast.expr.ExprImpl
import vct.col.ast.{TTuple, TupGet, Type}
import vct.col.check.{CheckContext, CheckError}

trait TupGetImpl[G] extends ExprImpl[G] { this: TupGet[G] =>
  def tupleType: TTuple[G] = tup.t.asTuple.get
  override def t: Type[G] = tupleType.elements(index)
  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) match {
      case Nil => if(0 <= index && index < tupleType.elements.size) Nil else Seq(col.check.TypeErrorText(this, _ => "Tuple getter exceeds tuple size"))
      case some => some
    }
}