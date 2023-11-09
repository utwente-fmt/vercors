package vct.col.ast.expr.op.bool

import vct.col.ast.{TBool, Type, SeqGuard}
import vct.col.print._

trait SeqGuardImpl[G] { this: SeqGuard[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.AND
  override def layout(implicit ctx: Ctx): Doc =
    Group(Doc.fold(conditions.map(_._2).map(assoc))(_ <+> "&&" <+/> _))
}
