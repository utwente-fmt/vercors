package vct.col.ast.lang

import vct.col.ast._
import vct.col.check.{CheckContext, CheckError}
import vct.col.print._

trait RuntimePostJoinImpl[G] {
  this: RuntimePostJoin[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("postjoin function")
}