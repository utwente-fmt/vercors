package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.CopyOnWriteArrayListContainsOps

trait CopyOnWriteArrayListContainsImpl[G] extends CopyOnWriteArrayListContainsOps[G] {
  this: CopyOnWriteArrayListContains[G] =>

  override def t: Type[G] = TBool[G]()

  override def layout(implicit ctx: Ctx): Doc =
    obj.show <> Text(".contains(") <> arg.show <> Text(")")

}