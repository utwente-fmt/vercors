package vct.col.ast.expr.context

import vct.col.ast.{TAnyClass, TChoreography, ThisChoreography, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.ThisChoreographyOps

trait ThisChoreographyImpl[G]
    extends ThisDeclarationImpl[G] with ThisChoreographyOps[G] {
  this: ThisChoreography[G] =>
  override def t: Type[G] = TChoreography(cls)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("this")
}
