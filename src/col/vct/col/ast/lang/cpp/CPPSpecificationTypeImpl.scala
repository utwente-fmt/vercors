package vct.col.ast.lang.cpp

import vct.col.ast.CPPSpecificationType
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.CPPSpecificationTypeOps

trait CPPSpecificationTypeImpl[G] extends CPPSpecificationTypeOps[G] { this: CPPSpecificationType[G] =>
  override def layout(implicit ctx: Ctx): Doc = t.show
}