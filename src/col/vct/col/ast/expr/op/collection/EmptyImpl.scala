package vct.col.ast.expr.op.collection

import vct.col.ast.{Empty, TBag, TBool, TMap, TSeq, TSet, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.EmptyOps

trait EmptyImpl[G] extends EmptyOps[G] {
  this: Empty[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = {
    ctx.syntax match {
      case Ctx.Isar =>
        obj.show <+> "=" <+>
          (obj.t match {
            case TSeq(_) => Text("[]")
            case TSet(_) => Text("{||}")
            case TBag(_) => Text("{#}")
            case TMap(_, _) => Text("fmempty")
          })
      case _ => assoc(obj) <> ".isEmpty"
    }

  }
}
