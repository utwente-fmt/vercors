package vct.col.ast.statement.veymont

import vct.col.ast.Communicate
import vct.col.print.{Ctx, Doc}

trait CommunicateImpl[G] { this: Communicate[G] =>
  override def layout(implicit ctx: Ctx): Doc = assign.show
}
