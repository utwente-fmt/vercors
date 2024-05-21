package vct.col.ast.expr.`type`

import vct.col.ast.{Cast, TType, Type}
import vct.col.check.UnreachableAfterTypeCheck
import vct.col.print.{Ctx, Doc, Group, Precedence, Show, Text}
import vct.col.ast.ops.CastOps

trait CastImpl[G] extends CastOps[G] { this: Cast[G] =>
  override def t: Type[G] = typeValue.t match {
    case TType(t) => t
    case t => t
//    case _ => throw UnreachableAfterTypeCheck("The cast type is not a type", this)
  }

  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("(") <> typeValue <> ")" <+> assoc(value)
}