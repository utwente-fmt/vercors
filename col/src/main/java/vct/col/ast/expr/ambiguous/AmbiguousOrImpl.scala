package vct.col.ast.expr.ambiguous

import vct.col.ast.{AmbiguousOr, TBool, TProcess, Type}
import vct.col.typerules.CoercionUtils

trait AmbiguousOrImpl[G] { this: AmbiguousOr[G] =>
  def isProcessOp: Boolean = CoercionUtils.getCoercion(left.t, TProcess()).isDefined
  override def t: Type[G] = if(isProcessOp) TProcess() else TBool()
}