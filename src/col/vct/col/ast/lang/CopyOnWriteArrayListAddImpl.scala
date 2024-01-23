package vct.col.ast.lang

import vct.col.ast._
import vct.col.print._

trait CopyOnWriteArrayListAddImpl[G] {
  this: CopyOnWriteArrayListAdd[G] =>

  override def t: Type[G] = TVoid[G]()

  override def layout(implicit ctx: Ctx): Doc =
    obj.show <> Text(".add(") <> arg.show <> Text(")")

}