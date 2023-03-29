package vct.col.ast.statement.composite

import vct.col.ast.util.Declarator
import vct.col.ast.{Declaration, VecBlock}

trait VecBlockImpl[G] extends Declarator[G] { this: VecBlock[G] =>
  override def declarations: Seq[Declaration[G]] = iters.map(_.variable)
}