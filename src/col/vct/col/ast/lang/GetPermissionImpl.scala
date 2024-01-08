package vct.col.ast.lang

import vct.col.ast._
import vct.col.print.{Empty, _}

trait GetPermissionImpl[G] {
  this: GetPermission[G] =>

  override def t: Type[G] = TBool[G]()

  override def layout(implicit ctx: Ctx): Doc =
    Group(Doc.arg(objectLocation) <> Text(s"__runtime__.get($id)") <> Text(".get(Thread.currentThread().threadId())"))
}