package vct.col.ast.expr.literal.build

import vct.col.ast.{LiteralBag, TBag, Type}
import vct.col.print._
import vct.col.ast.ops.LiteralBagOps

trait LiteralBagImpl[G] extends LiteralBagOps[G] { this: LiteralBag[G] =>
  override def t: Type[G] = TBag(element)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("bag<") <> element <> ">{" <> Doc.args(values) <> "}")
}