package vct.col.ast.node

import vct.col.ast.VerificationContext
import vct.col.print._

trait VerificationContextImpl[G] {
  this: VerificationContext[G] =>
  override def layout(implicit ctx: Ctx): Doc = program.show
}
