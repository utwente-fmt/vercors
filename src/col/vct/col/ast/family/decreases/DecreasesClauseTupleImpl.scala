package vct.col.ast.family.decreases

import vct.col.ast.DecreasesClauseTuple
import vct.col.print._

trait DecreasesClauseTupleImpl[G] { this: DecreasesClauseTuple[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("decreases") <+> Doc.args(exprs) <> ";")
}
