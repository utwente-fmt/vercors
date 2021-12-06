package vct.col.ast.temporaryimplpackage.lang

import hre.util.FuncTools
import vct.col.ast.{JavaNewLiteralArray, TArray, Type}

trait JavaNewLiteralArrayImpl { this: JavaNewLiteralArray =>
  override def t: Type = FuncTools.repeat(TArray(_), dims, baseType)
}