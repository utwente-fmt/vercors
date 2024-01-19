package vct.col.ast.expr.context

import vct.col.ast.{TAnyClass, TSeqProg, ThisSeqProg, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.ThisSeqProgOps

trait ThisSeqProgImpl[G] extends ThisDeclarationImpl[G] with ThisSeqProgOps[G] { this: ThisSeqProg[G] =>
  override def t: Type[G] = TSeqProg(cls)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("this")
}