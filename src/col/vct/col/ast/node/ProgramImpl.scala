package vct.col.ast.node

import vct.col.ast.{Node, Program}
import vct.col.ast.util.Declarator
import vct.col.check.{CheckContext, CheckError}
import vct.col.print.{Ctx, Doc}
import vct.col.util.CurrentProgramContext
import vct.result.VerificationError

import scala.collection.parallel.CollectionConverters._

trait ProgramImpl[G] extends Declarator[G] { this: Program[G] =>
  def check: Seq[CheckError] = checkTrans(CheckContext())

  override def checkContextRecursor[T](context: CheckContext[G], f: (CheckContext[G], Node[G]) => T): Seq[T] =
    VerificationError.context(CurrentProgramContext(this)) {
      super.checkContextRecursor(context, f)
    }

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(declarations)
}