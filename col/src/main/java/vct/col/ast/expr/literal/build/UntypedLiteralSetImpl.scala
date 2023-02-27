package vct.col.ast.expr.literal.build

import vct.col.ast.{TSet, Type, UntypedLiteralSet}
import vct.col.typerules.Types

trait UntypedLiteralSetImpl[G] { this: UntypedLiteralSet[G] =>
  lazy val elementType: Type[G] = Types.leastCommonSuperType(values.map(_.t))
  override def t: Type[G] = TSet(elementType)
}