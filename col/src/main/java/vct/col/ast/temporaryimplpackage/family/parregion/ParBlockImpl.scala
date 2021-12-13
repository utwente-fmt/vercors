package vct.col.ast.temporaryimplpackage.family.parregion

import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.ast.{Declaration, ParBlock}

trait ParBlockImpl[G] extends Declarator[G] { this: ParBlock[G] =>
  override def declarations: Seq[Declaration[G]] = iters.map(_.variable)
}