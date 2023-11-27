package vct.col.ast.lang

import vct.col.ast.{CPPExprOrTypeSpecifier, Expr, SYCLTConstructableClass}
import vct.col.resolve.ctx.CPPInvocationTarget

trait SYCLTConstructableClassImpl[G] { this: SYCLTConstructableClass[G] =>
  def findConstructor(genericArgs: Seq[CPPExprOrTypeSpecifier[G]], args: Seq[Expr[G]]): Option[CPPInvocationTarget[G]]
}