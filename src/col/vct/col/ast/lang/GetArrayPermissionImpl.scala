package vct.col.ast.lang

import vct.col.ast._
import vct.col.print._

trait GetArrayPermissionImpl[G] {
  this: GetArrayPermission[G] =>

  override def t: Type[G] = TBool[G]()

  override def layout(implicit ctx: Ctx): Doc =
    Group(Doc.arg(objectLocation) <> Text(s".__runtime__$id.get(") <> location <> Text(").get(Thread.currentThread().getId())"))
}