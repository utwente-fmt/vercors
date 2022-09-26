package vct.col.ast.lang

import hre.util.FuncTools
import vct.col.ast.{JavaNewLiteralArray, TArray, Type}

trait JavaNewLiteralArrayImpl[G] { this: JavaNewLiteralArray[G] =>
  override def t: Type[G] = FuncTools.repeat[Type[G]](TArray(_), dims, baseType)
}