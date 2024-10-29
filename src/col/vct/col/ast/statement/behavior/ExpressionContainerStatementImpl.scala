package vct.col.ast.statement.behavior

import vct.col.ast.{Expr, ExpressionContainerStatement}
import vct.col.ast.statement.StatementImpl

trait ExpressionContainerStatementImpl[G] extends StatementImpl[G] {
  this: ExpressionContainerStatement[G] =>
  def expr: Expr[G]
}
