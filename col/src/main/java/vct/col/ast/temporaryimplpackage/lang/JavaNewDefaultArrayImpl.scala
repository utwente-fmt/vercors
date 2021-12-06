package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaNewDefaultArray, TArray, Type}

trait JavaNewDefaultArrayImpl { this: JavaNewDefaultArray =>
  override def t: Type = (0 until (specifiedDims.size + moreDims)).foldLeft(baseType)((t, _) => TArray(t))
}