package vct.col.ast.expr.literal.build

import vct.col.ast.{TSeq, Type, UntypedLiteralSeq}
import vct.col.util.Types

trait UntypedLiteralSeqImpl[G] { this: UntypedLiteralSeq[G] =>
  def elementType: Type[G] = Types.leastCommonSuperType(values.map(_.t))
  override def t: Type[G] = TSeq(elementType)
}