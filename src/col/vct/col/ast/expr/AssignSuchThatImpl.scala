package vct.col.ast.expr

import vct.col.ast.{AssignSuchThat, Expr, Type}
import vct.col.ast.ops.AssignSuchThatOps
import vct.col.print._

trait AssignSuchThatImpl[G] extends AssignSuchThatOps[G] {
  this: AssignSuchThat[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(target.show <+> ":|" <>> constraint <+> ";")

  override def expr: Expr[G] = this.constraint
}
