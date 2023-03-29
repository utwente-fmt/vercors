package vct.col.ast.expr.op.option

import vct.col.ast.{OptGetOrElse, TOption, Type}
import vct.col.typerules.Types

trait OptGetOrElseImpl[G] { this: OptGetOrElse[G] =>
  def optionType: TOption[G] = opt.t.asOption.get
  override lazy val t: Type[G] = Types.leastCommonSuperType(optionType.element, alt.t)
}