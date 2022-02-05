package vct.col.ast.temporaryimplpackage.expr.ambiguous

import vct.col.ast._
import vct.col.coerce.CoercionUtils

trait AmbiguousMultImpl[G] { this: AmbiguousMult[G] =>
  def isProcessOp: Boolean = CoercionUtils.getCoercion(left.t, TProcess()).isDefined
  def isIntOp: Boolean = CoercionUtils.getCoercion(left.t, TInt()).isDefined && CoercionUtils.getCoercion(right.t, TInt()).isDefined

  override def t: Type[G] = if(isProcessOp) TProcess() else (if(isIntOp) TInt() else TRational())
}