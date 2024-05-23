package vct.col.ast.family.decreases

import vct.col.ast.DecreasesClauseTuple
import vct.col.print._
import vct.col.ast.ops.DecreasesClauseTupleOps

trait DecreasesClauseTupleImpl[G] extends DecreasesClauseTupleOps[G] { this: DecreasesClauseTuple[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("decreases") <+> Doc.args(exprs) <> ";")
}
