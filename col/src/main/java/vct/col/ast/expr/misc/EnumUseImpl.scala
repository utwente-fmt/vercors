package vct.col.ast.expr.misc

import vct.col.ast.{EnumUse, TEnum, Type}

trait EnumUseImpl[G] { this: EnumUse[G] =>
  override def t: Type[G] = TEnum(enum)
}
