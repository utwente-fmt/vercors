package vct.col.ast.temporaryimplpackage.statement.composite

import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.ast.{Declaration, VecBlock}

trait VecBlockImpl extends Declarator { this: VecBlock =>
  override def declarations: Seq[Declaration] = iters.map(_.variable)
}