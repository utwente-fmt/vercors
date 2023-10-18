package vct.col.ast.expr.op.tuple

import vct.col
import vct.col.ast.expr.ExprImpl
import vct.col.ast.{TTuple, TupGet, Type}
import vct.col.check.{CheckContext, CheckError}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait TupGetImpl[G] extends ExprImpl[G] {
  this: TupGet[G] =>
  def tupleType: TTuple[G] = tup.t.asTuple.get
  override def t: Type[G] = tupleType.elements(index)
  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) match {
      case Nil =>
        if (0 <= index && index < tupleType.elements.size)
          Nil
        else
          Seq(
            col.check
              .TypeErrorExplanation(this, "Tuple getter exceeds tuple size")
          )
      case some => some
    }

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    index match {
      case 0 => assoc(tup) <> ".fst"
      case 1 => assoc(tup) <> ".snd"
      case other => assoc(tup) <> "." <> other.toString
    }
}
