package vct.col.ast.expr.op.cmp

import vct.col.ast.{Eq, TBool}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.EqOps

trait EqImpl[G] extends EqOps[G] {
  this: Eq[G] =>
  override def precedence: Int = Precedence.EQUALITY
  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Isar =>
        left.t match {
          case TBool() =>
            Group(
              Text("(") <> assoc(left) <> ")" <+> "⟷" <+/> "(" <>
                nassoc(right) <> ")"
            )
          case _ => lassoc(left, "=", right)
        }
      case _ => lassoc(left, "==", right)
    }
}
