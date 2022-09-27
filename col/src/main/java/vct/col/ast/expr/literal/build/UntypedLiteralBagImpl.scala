package vct.col.ast.expr.literal.build

import vct.col.ast.{TBag, Type, UntypedLiteralBag}
import vct.col.util.Types

trait UntypedLiteralBagImpl[G] { this: UntypedLiteralBag[G] =>
  def elementType: Type[G] = Types.leastCommonSuperType(values.map(_.t))
  override def t: Type[G] = TBag(elementType)
}