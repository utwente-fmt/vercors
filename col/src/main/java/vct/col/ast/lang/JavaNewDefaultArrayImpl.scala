package vct.col.ast.lang

import vct.col.ast.{JavaNewDefaultArray, TArray, Type}

trait JavaNewDefaultArrayImpl[G] { this: JavaNewDefaultArray[G] =>
  override lazy val t: Type[G] = (0 until (specifiedDims.size + moreDims)).foldLeft(baseType)((t, _) => TArray(t))
}