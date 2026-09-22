package vct.col.ast.lang.llvm

import vct.col.ast.LLVMEval
import vct.col.ast.ops.LLVMEvalOps
import vct.col.ast.statement.StatementImpl
import vct.col.print.{Ctx, Doc}

trait LLVMEvalImpl[G] extends StatementImpl[G] with LLVMEvalOps[G] {
  this: LLVMEval[G] =>

  override def layout(implicit ctx: Ctx): Doc = expr.show <> ";"
}
