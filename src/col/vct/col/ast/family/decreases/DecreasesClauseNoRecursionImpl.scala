package vct.col.ast.family.decreases

import vct.col.ast.DecreasesClauseNoRecursion
import vct.col.print.{Ctx, Doc, Text}

trait DecreasesClauseNoRecursionImpl[G] {
  this: DecreasesClauseNoRecursion[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("decreases;")
}
