package vct.col.ast.node

import vct.col.ast.Verification
import vct.col.check.{CheckContext, CheckError}
import vct.col.print._

trait VerificationImpl[G] { this: Verification[G] =>
  def check: Seq[CheckError] = checkTrans(CheckContext())

  override def layout(implicit ctx: Ctx): Doc =
    if(tasks.size == 1) tasks.head.show
    else Doc.stack(tasks.zipWithIndex.map {
      case (task, i) => Text(s"/* Verification task ${i+1} */") <+/> task.show
    })
}
