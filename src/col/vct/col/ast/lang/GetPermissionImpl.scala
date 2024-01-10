package vct.col.ast.lang

import vct.col.ast._
import vct.col.print.{Empty, _}

trait GetPermissionImpl[G] {
  this: GetPermission[G] =>

  override def t: Type[G] = TFraction[G]()

  override def layout(implicit ctx: Ctx): Doc =
    Nest(objectLocation.show <> Text(s".__runtime__.get($id).get(Thread.currentThread().threadId())"))
}