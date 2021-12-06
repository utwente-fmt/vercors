package vct.col.ast.temporaryimplpackage.expr.op.option

import vct.col.ast.{OptGetOrElse, TOption, Type}
import vct.col.util.Types

trait OptGetOrElseImpl { this: OptGetOrElse =>
  def optionType: TOption = opt.t.asOption.get
  override def t: Type = Types.leastCommonSuperType(optionType.element, alt.t)
}