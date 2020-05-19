package vct.col.util

import vct.col.ast.expr.BindingExpression
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.{ASTVisitor, RecursiveVisitor}
import hre.lang.System._

case class FindBinders(override val source: ProgramUnit) extends RecursiveVisitor[Unit](source) {
  override def visit(exp: BindingExpression): Unit = {
    Output("[binder] %s", exp)
  }
}
