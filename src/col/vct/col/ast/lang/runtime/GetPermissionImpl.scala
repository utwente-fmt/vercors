package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.GetPermissionOps

trait GetPermissionImpl[G] extends GetPermissionOps[G] {
  this: GetPermission[G] =>

  override def t: Type[G] = TFraction[G]()

  private def defaultFraction: RuntimeFractionZero[G] = RuntimeFractionZero[G]()


  override def layout(implicit ctx: Ctx): Doc =
    Nest(objectLocation.show <> Text(s".__runtime__.get($id).getOrDefault(") <> threadId.show <> Text(",") <+> defaultFraction.show <> Text(")"))
}