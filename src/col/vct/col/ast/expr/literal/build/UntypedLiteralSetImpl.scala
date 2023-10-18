package vct.col.ast.expr.literal.build

import vct.col.ast.{TSet, Type, UntypedLiteralSet}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.typerules.Types

trait UntypedLiteralSetImpl[G] {
  this: UntypedLiteralSet[G] =>
  lazy val elementType: Type[G] = Types.leastCommonSuperType(values.map(_.t))
  override def t: Type[G] = TSet(elementType)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("{") <> Doc.args(values) <> "}")
}
