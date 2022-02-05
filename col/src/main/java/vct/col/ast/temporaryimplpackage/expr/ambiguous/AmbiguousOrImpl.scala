package vct.col.ast.temporaryimplpackage.expr.ambiguous

import vct.col.ast.{AmbiguousOr, TBool, TProcess, Type}
import vct.col.coerce.CoercionUtils

trait AmbiguousOrImpl[G] { this: AmbiguousOr[G] =>
  def isProcessOp: Boolean = CoercionUtils.getCoercion(left.t, TProcess()).isDefined
  override def t: Type[G] = if(isProcessOp) TProcess() else TBool()
}