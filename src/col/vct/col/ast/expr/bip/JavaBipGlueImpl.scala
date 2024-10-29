package vct.col.ast.expr.bip

import vct.col.ast.{JavaBipGlue, TNotAValue, Type}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.JavaBipGlueOps

trait JavaBipGlueImpl[G] extends JavaBipGlueOps[G] {
  this: JavaBipGlue[G] =>
  override def t: Type[G] = new TNotAValue()

  override def layout(implicit ctx: Ctx): Doc = Doc.stack(elems)
}
