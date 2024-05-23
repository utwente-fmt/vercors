package vct.col.ast.node

import vct.col.ast.VerificationContext
import vct.col.print._
import vct.col.ast.ops.{VerificationContextOps, VerificationContextFamilyOps}

trait VerificationContextImpl[G]
    extends VerificationContextOps[G] with VerificationContextFamilyOps[G] {
  this: VerificationContext[G] =>
  override def layout(implicit ctx: Ctx): Doc = program.show
}
