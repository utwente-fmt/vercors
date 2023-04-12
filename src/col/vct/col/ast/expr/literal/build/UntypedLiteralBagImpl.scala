package vct.col.ast.expr.literal.build

import vct.col.ast.{TBag, Type, UntypedLiteralBag}
import vct.col.print._
import vct.col.typerules.Types

trait UntypedLiteralBagImpl[G] { this: UntypedLiteralBag[G] =>
  lazy val elementType: Type[G] = Types.leastCommonSuperType(values.map(_.t))
  override def t: Type[G] = TBag(elementType)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("b{") <> Doc.args(values) <> "}")
}