package vct.col.ast.temporaryimplpackage.family.catchclause

import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.ast.{CatchClause, Declaration}

trait CatchClauseImpl extends Declarator { this: CatchClause =>
  override def declarations: Seq[Declaration] = Seq(decl)
}