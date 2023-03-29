package vct.col.ast.family.signals

import vct.col.ast.{Declaration, SignalsClause}
import vct.col.ast.util.Declarator

trait SignalsClauseImpl[G] extends Declarator[G] { this: SignalsClause[G] =>
  override def declarations: Seq[Declaration[G]] = Seq(binding)
}