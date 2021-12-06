package vct.col.ast.temporaryimplpackage.statement.composite

import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.ast.{Declaration, ParInvariant}

trait ParInvariantImpl extends Declarator { this: ParInvariant =>
  override def declarations: Seq[Declaration] = Seq(decl)
}