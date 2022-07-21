package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{CConstant, Type, TFloat, TInt}

trait CConstantImpl[G] { this: CConstant[G] =>
  override def t: Type[G] = if (isInt()) TInt[G] else if (isFloat()) TFloat[G] else TInt[G]; // FIXME what if it is no float nor int?

  def isFloat() : Boolean = {!value.toFloatOption.isEmpty}

  def isInt() : Boolean = {!value.toIntOption.isEmpty}
}