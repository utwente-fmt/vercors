package vct.col.ast.node

import vct.col.ast.{Node, Program}
import vct.col.ast.util.Declarator
import vct.col.check.{CheckContext, CheckError}
import vct.col.print.{Ctx, Doc, Empty, Line, Text}
import vct.col.util.CurrentCheckProgramContext
import vct.result.VerificationError
import vct.col.ast.ops.{ProgramFamilyOps, ProgramOps}

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

  def layoutIsar(implicit ctx: Ctx): Doc = {
    Text("theory") <+> Text(ctx.theoryName) </> Text("imports Main HOL.Rat") </>
      Text("begin") </> Text("typedecl ref") </> Doc.stack2(declarations) </>
      Text("end")
  }

  override def layout(implicit ctx: Ctx): Doc = {
    ctx.syntax match {
      case Ctx.Isar => layoutIsar
      case _ =>
        (if (ctx.syntax == Ctx.Java)
           (Text("import java.util.concurrent.locks.Lock;") <+/>
             "import java.util.concurrent.locks.ReentrantLock;" <+/>
             "import java.util.concurrent.locks.Condition;" <> Line)
         else
           Empty) <> Doc.stack2(declarations)
    }
  }
}
