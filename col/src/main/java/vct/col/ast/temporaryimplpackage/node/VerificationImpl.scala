package vct.col.ast.temporaryimplpackage.node

import vct.col.ast.Verification
import vct.col.check.{CheckContext, CheckError}

trait VerificationImpl[G] { this: Verification[G] =>
  def check: Seq[CheckError] = checkTrans(CheckContext())
}
