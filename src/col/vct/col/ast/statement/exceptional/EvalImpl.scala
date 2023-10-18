package vct.col.ast.statement.exceptional

import vct.col.ast.Eval
import vct.col.print.{Ctx, Doc}

trait EvalImpl[G] {
  this: Eval[G] =>
  override def layout(implicit ctx: Ctx): Doc = expr.show <> ";"
}
