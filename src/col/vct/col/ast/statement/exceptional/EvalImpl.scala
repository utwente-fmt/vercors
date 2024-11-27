package vct.col.ast.statement.exceptional

import vct.col.ast.statement.StatementImpl
import vct.col.ast.Eval
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.EvalOps

trait EvalImpl[G] extends StatementImpl[G] with EvalOps[G] {
  this: Eval[G] =>
  override def layout(implicit ctx: Ctx): Doc = expr.show <> ";"
}
