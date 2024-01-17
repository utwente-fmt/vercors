package vct.col.ast.lang.c

import vct.col.ast.CSpecificationType
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CSpecificationTypeOps

trait CSpecificationTypeImpl[G] extends CSpecificationTypeOps[G] { this: CSpecificationType[G] =>
  override def layout(implicit ctx: Ctx): Doc = t.show
}