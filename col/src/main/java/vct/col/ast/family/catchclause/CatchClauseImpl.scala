package vct.col.ast.family.catchclause

import vct.col.ast.util.Declarator
import vct.col.ast.{CatchClause, Declaration}

trait CatchClauseImpl[G] extends Declarator[G] { this: CatchClause[G] =>
  override def declarations: Seq[Declaration[G]] = Seq(decl)
}