package vct.col.ast.expr.literal.build

import vct.col.ast.{TBag, Type, UntypedLiteralBag}
import vct.col.print._
import vct.col.typerules.Types
import vct.col.ast.ops.UntypedLiteralBagOps

trait UntypedLiteralBagImpl[G] extends UntypedLiteralBagOps[G] {
  this: UntypedLiteralBag[G] =>
  lazy val elementType: Type[G] = Types.leastCommonSuperType(values.map(_.t))
  override def t: Type[G] = TBag(elementType)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = {
    ctx.syntax match {
      case Ctx.Isar => Group(Text("{#") <+> Doc.args(values) <+> Text("#}"))
      case _ => Group(Text("b{") <> Doc.args(values) <> "}")
    }
  }
}
