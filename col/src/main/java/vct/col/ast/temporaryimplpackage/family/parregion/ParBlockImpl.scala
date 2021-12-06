package vct.col.ast.temporaryimplpackage.family.parregion

import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.ast.{Declaration, ParBlock}

trait ParBlockImpl extends Declarator { this: ParBlock =>
  override def declarations: Seq[Declaration] = iters.map(_.variable)
}