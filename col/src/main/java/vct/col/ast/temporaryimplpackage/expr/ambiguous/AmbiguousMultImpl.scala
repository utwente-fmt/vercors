package vct.col.ast.temporaryimplpackage.expr.ambiguous

import vct.col.ast._
import vct.col.coerce.Coercion

trait AmbiguousMultImpl { this: AmbiguousMult =>
  def isProcessOp: Boolean = Coercion.getCoercion(left.t, TProcess()).isDefined
  def isIntOp: Boolean = Coercion.getCoercion(left.t, TInt()).isDefined && Coercion.getCoercion(right.t, TInt()).isDefined

  override def t: Type = if(isProcessOp) TProcess() else (if(isIntOp) TInt() else TRational())
}