package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaNewDefaultArray, TArray, Type}

trait JavaNewDefaultArrayImpl[G] { this: JavaNewDefaultArray[G] =>
  override def t: Type[G] = (0 until (specifiedDims.size + moreDims)).foldLeft(baseType)((t, _) => TArray(t))
}