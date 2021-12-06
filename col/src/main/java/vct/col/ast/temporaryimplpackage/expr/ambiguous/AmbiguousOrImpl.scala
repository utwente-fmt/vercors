package vct.col.ast.temporaryimplpackage.expr.ambiguous

import vct.col.ast.{AmbiguousOr, TBool, TProcess, Type}
import vct.col.coerce.Coercion

trait AmbiguousOrImpl { this: AmbiguousOr =>
  def isProcessOp: Boolean = Coercion.getCoercion(left.t, TProcess()).isDefined
  override def t: Type = if(isProcessOp) TProcess() else TBool()
}