package vct.col.ast.expr.op.collection

import vct.col.ast.{Size, TBag, TInt, TMap, TSeq, TSet, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.SizeOps

trait SizeImpl[G] extends SizeOps[G] {
  this: Size[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = {
    ctx.syntax match {
      case Ctx.Isar =>
        obj.t match {
          case TSeq(_) => Text("int(length") <+> obj <> ")"
          case TSet(_) => Text("int(fcard") <+> obj <> ")"
          case TBag(_) => Text("int(size_multiset") <+> obj <> ")"
          case TMap(_, _) => Text("int(fcard (fmdom") <+> obj <> "))"
        }
      case _ => Text("|") <> obj <> "|"
    }
  }
}
