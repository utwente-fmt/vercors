package vct.col.ast.temporaryimplpackage.statement.composite

import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.ast.{Declaration, ParInvariant}

trait ParInvariantImpl[G] extends Declarator[G] { this: ParInvariant[G] =>
  override def declarations: Seq[Declaration[G]] = Seq(decl)
}