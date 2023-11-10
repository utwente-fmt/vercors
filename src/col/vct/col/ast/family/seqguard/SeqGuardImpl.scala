package vct.col.ast.family.seqguard

import vct.col.ast.{Expr, SeqGuard}

trait SeqGuardImpl[G] { this: SeqGuard[G] =>
  def condition: Expr[G]
}
