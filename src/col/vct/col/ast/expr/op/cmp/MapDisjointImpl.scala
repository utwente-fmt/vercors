package vct.col.ast.expr.op.cmp

import vct.col.ast.{MapDisjoint, TBool, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.MapDisjointOps

trait MapDisjointImpl[G] extends MapDisjointOps[G] {
  this: MapDisjoint[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = {
    ctx.syntax match {
      case Ctx.Isar => Group(Text("dom") <+> assoc(left) <+> Text("∩") <+> Text("dom") <+> assoc(right) <+> Text("= {}") )
      case _ => Group(assoc(left) <> ".disjoint(" <> Doc.arg(right) <> ")")
    }
  }
}
