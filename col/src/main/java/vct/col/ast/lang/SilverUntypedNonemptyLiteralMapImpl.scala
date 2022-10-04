package vct.col.ast.lang

import vct.col.ast.{Expr, SilverUntypedNonemptyLiteralMap, TMap, Type}
import vct.col.typerules.Types

trait SilverUntypedNonemptyLiteralMapImpl[G] { this: SilverUntypedNonemptyLiteralMap[G] =>
  def mapKeys: Seq[Expr[G]] = values.map(_._1)
  def mapValues: Seq[Expr[G]] = values.map(_._2)

  def keyType: Type[G] = Types.leastCommonSuperType(mapKeys.map(_.t))
  def valueType: Type[G] = Types.leastCommonSuperType(mapValues.map(_.t))

  override def t: TMap[G] = TMap(keyType, valueType)
}
