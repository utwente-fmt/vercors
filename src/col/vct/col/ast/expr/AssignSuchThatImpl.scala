package vct.col.ast.expr

import vct.col.ast.{AssignSuchThat, Type}
import vct.col.ast.ops.AssignSuchThatOps
import vct.col.print._

trait AssignSuchThatImpl[G] extends AssignSuchThatOps[G] {
  this: AssignSuchThat[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(target.show <+> ":|" <>> constraint <+> ";")
}
