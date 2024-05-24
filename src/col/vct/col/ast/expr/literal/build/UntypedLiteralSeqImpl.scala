package vct.col.ast.expr.literal.build

import vct.col.ast.{TSeq, Type, UntypedLiteralSeq}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.typerules.Types
import vct.col.ast.ops.UntypedLiteralSeqOps

trait UntypedLiteralSeqImpl[G] extends UntypedLiteralSeqOps[G] { this: UntypedLiteralSeq[G] =>
  lazy val elementType: Type[G] = Types.leastCommonSuperType(values.map(_.t))
  override def t: Type[G] = TSeq(elementType)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("[") <> Doc.args(values) <> "]")
}