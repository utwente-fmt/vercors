package vct.col.ast.lang

import vct.col.ast.CPPSpecificationType
import vct.col.print.{Ctx, Doc}

trait CPPSpecificationTypeImpl[G] { this: CPPSpecificationType[G] =>
  override def layout(implicit ctx: Ctx): Doc = t.show
}