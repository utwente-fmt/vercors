package vct.col.ast.declaration.global

import vct.col.ast.ExpectedError
import vct.col.print.{Ctx, Doc, Empty}

trait ExpectedErrorImpl[G] { this: ExpectedError[G] =>
  override def layout(implicit ctx: Ctx): Doc = Empty
}
