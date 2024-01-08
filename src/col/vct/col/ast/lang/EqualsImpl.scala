package vct.col.ast.lang

import vct.col.ast._
import vct.col.print.{Empty, _}

trait EqualsImpl[G] {
  this: Equals[G] =>

  override def t: Type[G] = TBool[G]()

  override def layout(implicit ctx: Ctx): Doc =
    obj.show <> Text(".equals(") <> target.show <> Text(")")
}