package vct.col.ast.expr.model

import vct.col.ast.{ModelMerge, TVoid, Type}
import vct.col.print.{Ctx, Doc, Precedence, Group}
import vct.col.ast.ops.ModelMergeOps

trait ModelMergeImpl[G] extends ModelMergeOps[G] { this: ModelMerge[G] =>
  override def t: Type[G] = TVoid()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(model) <> "." <> "merge" <> "(" <> Doc.args(Seq(leftPerm, leftProcess, rightPerm, rightProcess)) <> ")")
}