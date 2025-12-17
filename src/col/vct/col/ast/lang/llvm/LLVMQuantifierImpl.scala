package vct.col.ast.lang.llvm

import vct.col.ast.expr.ExprImpl
import vct.col.ast.{Expr, LLVMQuantifier, TBool, Type}

trait LLVMQuantifierImpl[G] extends ExprImpl[G] {
  this: LLVMQuantifier[G] =>
  def bindingExpr: Expr[G]
  def bodyExpr: Expr[G]

  override def t: Type[G] = TBool()

}
