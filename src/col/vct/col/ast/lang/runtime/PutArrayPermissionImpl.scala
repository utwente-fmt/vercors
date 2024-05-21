package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.PutArrayPermissionOps

trait PutArrayPermissionImpl[G] extends PutArrayPermissionOps[G] {
  this: PutArrayPermission[G] =>

  override def precedence: Int = Precedence.ATOMIC

  override def t: Type[G] = TFraction[G]()

  override def layout(implicit ctx: Ctx): Doc =
    Nest(objectLocation.show <> Text(s".__runtime__$id.get(") <> location <> Text(").put(") <> threadId.show <> Text(", ") <> permission <> Text(")"))
}