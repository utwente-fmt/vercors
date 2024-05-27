package vct.col.ast.expr.heap.read

import vct.col.ast.{NdIndex, TInt}
import vct.col.print._
import vct.col.ast.ops.NdIndexOps

trait NdIndexImpl[G] extends NdIndexOps[G] {
  this: NdIndex[G] =>
  override def t: TInt[G] = TInt()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("\\nd_index(") <> Doc.args(indices.zip(dimensions).map {
      case (idx, dim) => idx.show <> "," <+> dim
    }) <> ")")
}
