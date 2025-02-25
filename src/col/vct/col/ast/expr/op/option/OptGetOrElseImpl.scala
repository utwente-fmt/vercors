package vct.col.ast.expr.op.option

import vct.col.ast.{OptGetOrElse, TOption, Type}
import vct.col.print.{Ctx, Doc, Precedence, Group}
import vct.col.typerules.Types
import vct.col.ast.ops.OptGetOrElseOps

trait OptGetOrElseImpl[G] extends OptGetOrElseOps[G] {
  this: OptGetOrElse[G] =>
  def optionType: TOption[G] = opt.t.asOption.get
  override lazy val t: Type[G] = Types
    .leastCommonSuperType(optionType.element, alt.t)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(opt) <> ".getOrElse(" <> Doc.arg(alt) <> ")")
}
