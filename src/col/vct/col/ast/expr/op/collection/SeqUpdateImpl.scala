package vct.col.ast.expr.op.collection

import vct.col.ast.{SeqUpdate, TSeq}
import vct.col.print._
import vct.col.typerules.Types
import vct.col.ast.ops.SeqUpdateOps

trait SeqUpdateImpl[G] extends SeqUpdateOps[G] { this: SeqUpdate[G] =>
  def tailType: TSeq[G] = xs.t.asSeq.get

  override lazy val t: TSeq[G] = TSeq(Types.leastCommonSuperType(tailType.element, x.t))

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(xs) <> ".update(" <> Doc.args(Seq(i, x)) <> ")")
}