package vct.col.ast.lang

import vct.col.ast.CSpecificationType
import vct.col.print.{Ctx, Doc, Text}

trait CSpecificationTypeImpl[G] { this: CSpecificationType[G] =>
  override def layout(implicit ctx: Ctx): Doc = t.show
}