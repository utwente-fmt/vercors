package vct.col.ast.expr.literal.build

import vct.col.ast.{TSet, Type, UntypedLiteralSet}
import vct.col.util.Types

trait UntypedLiteralSetImpl[G] { this: UntypedLiteralSet[G] =>
  def elementType: Type[G] = Types.leastCommonSuperType(values.map(_.t))
  override def t: Type[G] = TSet(elementType)
}