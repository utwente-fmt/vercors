package vct.col.ast.statement.composite

import vct.col.ast.util.Declarator
import vct.col.ast.{Declaration, ParInvariant}

trait ParInvariantImpl[G] extends Declarator[G] { this: ParInvariant[G] =>
  override def declarations: Seq[Declaration[G]] = Seq(decl)
}