package vct.col.ast.expr.veymont

import vct.col.ast.ops.ChorPermOps
import vct.col.ast.{ChorPerm, TResource, Type}
import vct.col.print._

trait ChorPermImpl[G] extends ChorPermOps[G] {
  this: ChorPerm[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("Perm") <> "[" <> ctx.name(endpoint) <> "]" <> "(" <> loc <> ", " <>
      perm <> ")"

  override def t: Type[G] = TResource()
}
