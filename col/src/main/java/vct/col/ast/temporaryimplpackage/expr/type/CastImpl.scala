package vct.col.ast.temporaryimplpackage.expr.`type`

import vct.col.ast.{Cast, TType, Type}
import vct.col.check.UnreachableAfterTypeCheck

trait CastImpl[G] { this: Cast[G] =>
  override def t: Type[G] = typeValue.t match {
    case TType(t) => t
    case _ => throw UnreachableAfterTypeCheck("The cast type is not a type", this)
  }
}