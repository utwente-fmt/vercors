package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.CopyOnWriteArrayListAddOps

trait CopyOnWriteArrayListAddImpl[G] extends CopyOnWriteArrayListAddOps[G] {
  this: CopyOnWriteArrayListAdd[G] =>

  override def t: Type[G] = TBool[G]()

  override def layout(implicit ctx: Ctx): Doc =
    obj.show <> Text(".add(") <> arg.show <> Text(")")

}