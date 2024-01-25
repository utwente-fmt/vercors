package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait PutPermissionImpl[G] {
  this: PutPermission[G] =>

  override def t: Type[G] = TFraction[G]()

  override def layout(implicit ctx: Ctx): Doc =
    Nest(objectLocation.show <> Text(s".__runtime__.get($id)") <> Text(".put(") <> threadId.show <> Text(",") <> Nest(permission.show) <> Text(")"))

}