package vct.col.ast.temporaryimplpackage.expr.ambiguous

import vct.col.ast._
import vct.col.coerce.Coercion
import vct.col.util.Types

trait AmbiguousPlusImpl { this: AmbiguousPlus =>
  def isProcessOp: Boolean = Coercion.getCoercion(left.t, TProcess()).isDefined
  def isSeqOp: Boolean = Coercion.getAnySeqCoercion(left.t).isDefined
  def isBagOp: Boolean = Coercion.getAnyBagCoercion(left.t).isDefined
  def isSetOp: Boolean = Coercion.getAnySetCoercion(left.t).isDefined
  def isPointerOp: Boolean = Coercion.getAnyPointerCoercion(left.t).isDefined
  def isIntOp: Boolean =
    Coercion.getCoercion(left.t, TInt()).isDefined &&
      Coercion.getCoercion(right.t, TInt()).isDefined

  override def t: Type =
    if(isProcessOp) TProcess()
    else if(isSeqOp || isBagOp || isSetOp) Types.leastCommonSuperType(left.t, right.t)
    else if(isPointerOp) left.t
    else if(isIntOp) TInt()
    else TRational()
}