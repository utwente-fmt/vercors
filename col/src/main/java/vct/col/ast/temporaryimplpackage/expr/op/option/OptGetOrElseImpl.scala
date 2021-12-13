package vct.col.ast.temporaryimplpackage.expr.op.option

import vct.col.ast.{OptGetOrElse, TOption, Type}
import vct.col.util.Types

trait OptGetOrElseImpl[G] { this: OptGetOrElse[G] =>
  def optionType: TOption[G] = opt.t.asOption.get
  override def t: Type[G] = Types.leastCommonSuperType(optionType.element, alt.t)
}