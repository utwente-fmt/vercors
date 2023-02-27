package vct.col.ast.expr.literal.build

import vct.col.ast.{TBag, Type, UntypedLiteralBag}
import vct.col.typerules.Types

trait UntypedLiteralBagImpl[G] { this: UntypedLiteralBag[G] =>
  lazy val elementType: Type[G] = Types.leastCommonSuperType(values.map(_.t))
  override def t: Type[G] = TBag(elementType)
}