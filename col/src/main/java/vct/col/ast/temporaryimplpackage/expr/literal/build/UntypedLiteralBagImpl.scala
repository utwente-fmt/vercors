package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{TBag, Type, UntypedLiteralBag}
import vct.col.util.Types

trait UntypedLiteralBagImpl { this: UntypedLiteralBag =>
  def elementType: Type = Types.leastCommonSuperType(values.map(_.t))
  override def t: Type = TBag(elementType)
}