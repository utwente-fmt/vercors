package vct.col.ast.lang

import vct.col.ast._
import vct.col.print._

trait CopyOnWriteArrayListRemoveImpl[G] {
  this: CopyOnWriteArrayListRemove[G] =>

  override def t: Type[G] = TBool[G]()

  override def layout(implicit ctx: Ctx): Doc =
    obj.show <> Text(".remove(") <> arg.show <> Text(")")

}