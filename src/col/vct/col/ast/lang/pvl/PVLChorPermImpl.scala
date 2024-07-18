package vct.col.ast.lang.pvl

import vct.col.ast.ops.PVLChorPermOps
import vct.col.ast.{PVLChorPerm, TResource, Type}
import vct.col.print._

trait PVLChorPermImpl[G] extends PVLChorPermOps[G] {
  this: PVLChorPerm[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("Perm") <> "[" <> endpoint <> "]" <> "(" <> loc <> ", " <> perm <> ")"

  override def t: Type[G] = TResource()
}
