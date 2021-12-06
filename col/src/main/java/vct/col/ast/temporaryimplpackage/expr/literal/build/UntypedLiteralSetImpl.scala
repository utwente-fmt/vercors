package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{TSet, Type, UntypedLiteralSet}
import vct.col.util.Types

trait UntypedLiteralSetImpl { this: UntypedLiteralSet =>
  def elementType: Type = Types.leastCommonSuperType(values.map(_.t))
  override def t: Type = TSet(elementType)
}