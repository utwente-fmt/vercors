package vct.col.ast.expr.`type`

import vct.col.ast.ops.IntegerPointerCastOps
import vct.col.ast.{IntegerPointerCast, TType, Type}
import vct.col.check.UnreachableAfterTypeCheck
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait IntegerPointerCastImpl[G] extends IntegerPointerCastOps[G] {
  this: IntegerPointerCast[G] =>
  override def t: Type[G] =
    typeValue.t match {
      case TType(t) => t
      case _ =>
        throw UnreachableAfterTypeCheck("The cast type is not a type", this)
    }

  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("(") <> typeValue <> ")" <> assoc(value)
}
