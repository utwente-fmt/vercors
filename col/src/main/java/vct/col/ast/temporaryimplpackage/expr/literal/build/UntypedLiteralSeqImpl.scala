package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{TSeq, Type, UntypedLiteralSeq}
import vct.col.util.Types

trait UntypedLiteralSeqImpl { this: UntypedLiteralSeq =>
  def elementType: Type = Types.leastCommonSuperType(values.map(_.t))
  override def t: Type = TSeq(elementType)
}