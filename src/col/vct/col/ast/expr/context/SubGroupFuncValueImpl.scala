package vct.col.ast.expr.context

import vct.col.ast.ops.SubGroupFuncValueOps
import vct.col.ast.{SubGroupFuncValue, TInt}
import vct.col.print.{Ctx, Doc, Precedence, Text}


trait SubGroupFuncValueImpl[G] extends SubGroupFuncValueOps[G] { this: SubGroupFuncValue[G] =>
  override def t: TInt[G] = TInt()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("\\sg_val")
}
