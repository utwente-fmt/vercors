package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.CopyOnWriteArrayListRemoveOps

trait CopyOnWriteArrayListRemoveImpl[G] extends CopyOnWriteArrayListRemoveOps[G] {
  this: CopyOnWriteArrayListRemove[G] =>

  override def t: Type[G] = TBool[G]()

  override def layout(implicit ctx: Ctx): Doc =
    obj.show <> Text(".remove(") <> arg.show <> Text(")")

}