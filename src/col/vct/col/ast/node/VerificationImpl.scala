package vct.col.ast.node

import vct.col.ast.Verification
import vct.col.check.{CheckContext, CheckError}
import vct.col.print._
import vct.col.ast.ops.{VerificationOps, VerificationFamilyOps}

trait VerificationImpl[G]
    extends VerificationOps[G] with VerificationFamilyOps[G] {
  this: Verification[G] =>
  def check: Seq[CheckError] = checkTrans(CheckContext())

  override def layout(implicit ctx: Ctx): Doc =
    if (tasks.size == 1)
      tasks.head.show
    else
      Doc.stack(tasks.zipWithIndex.map { case (task, i) =>
        Text(s"/* Verification task ${i + 1} */") <+/> task.show
      })
}
