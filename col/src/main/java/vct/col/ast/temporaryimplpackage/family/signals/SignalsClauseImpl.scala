package vct.col.ast.temporaryimplpackage.family.signals

import vct.col.ast.{Declaration, SignalsClause}
import vct.col.ast.temporaryimplpackage.util.Declarator

trait SignalsClauseImpl extends Declarator { this: SignalsClause =>
  override def declarations: Seq[Declaration] = Seq(binding)
}