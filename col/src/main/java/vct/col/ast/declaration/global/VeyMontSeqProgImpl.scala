package vct.col.ast.declaration.global

import vct.col.ast.{Class, Declaration, VeyMontSeqProg}
import vct.col.ast.util.Declarator
import vct.col.check.{CheckContext, CheckError}
import vct.col.origin.Origin

trait VeyMontSeqProgImpl[G] extends Declarator[G] { this: VeyMontSeqProg[G] =>
  def members: Seq[Declaration[G]] = threads ++ Seq(runMethod) ++ methods
  override def declarations: Seq[Declaration[G]] = progArgs ++ members
}
