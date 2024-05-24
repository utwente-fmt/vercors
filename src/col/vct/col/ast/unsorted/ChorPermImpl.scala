package vct.col.ast.unsorted

import vct.col.ast.{ChorPerm, TResource, Type}
import vct.col.ast.ops.ChorPermOps
import vct.col.print._

trait ChorPermImpl[G] extends ChorPermOps[G] { this: ChorPerm[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("Perm") <> "[" <> ctx.name(endpoint) <> "]" <> "(" <> loc <> ", " <> perm <> ")"

  override def t: Type[G] = TResource()
}
