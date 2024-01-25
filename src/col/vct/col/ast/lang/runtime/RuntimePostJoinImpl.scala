package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait RuntimePostJoinImpl[G] {
  this: RuntimePostJoin[G] =>

  override def layout(implicit ctx: Ctx): Doc = Doc.spec(obj.show <> ".postJoin(" <> arg.show <> ")")
}