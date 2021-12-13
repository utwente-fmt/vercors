package vct.col.ast.temporaryimplpackage.family.signals

import vct.col.ast.{Declaration, SignalsClause}
import vct.col.ast.temporaryimplpackage.util.Declarator

trait SignalsClauseImpl[G] extends Declarator[G] { this: SignalsClause[G] =>
  override def declarations: Seq[Declaration[G]] = Seq(binding)
}