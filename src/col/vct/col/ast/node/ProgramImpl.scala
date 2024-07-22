package vct.col.ast.node

import vct.col.ast.{Node, Program}
import vct.col.ast.util.Declarator
import vct.col.check.{CheckContext, CheckError}
import vct.col.print.{Ctx, Doc}
import vct.col.util.CurrentCheckProgramContext
import vct.result.VerificationError
import vct.col.ast.ops.{ProgramOps, ProgramFamilyOps}

trait ProgramImpl[G]
    extends Declarator[G] with ProgramOps[G] with ProgramFamilyOps[G] {
  this: Program[G] =>
  def check: Seq[CheckError] = checkTrans(CheckContext())

  override def checkContextRecursor[T](
      context: CheckContext[G],
      f: (CheckContext[G], Node[G]) => T,
  ): Seq[T] =
    VerificationError.withContext(CurrentCheckProgramContext(this)) {
      super.checkContextRecursor(context, f)
    }

  override def layout(implicit ctx: Ctx): Doc =
    Doc.fold(declarations)(_ <> vct.col.print.Line <> vct.col.print.Line <> _)
}
