package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait CopyOnWriteArrayListContainsImpl[G] {
  this: CopyOnWriteArrayListContains[G] =>

  override def t: Type[G] = TBool[G]()

  override def layout(implicit ctx: Ctx): Doc =
    obj.show <> Text(".contains(") <> arg.show <> Text(")")

}