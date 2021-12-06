package vct.col.ast.temporaryimplpackage.node

import vct.col.ast.Program
import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.check.{CheckContext, CheckError}

trait ProgramImpl extends Declarator { this: Program =>
  def check: Seq[CheckError] =
    checkTrans(CheckContext())

  override def check(context: CheckContext): Seq[CheckError] = Nil
}