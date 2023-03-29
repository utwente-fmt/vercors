package vct.col.ast.lang

import vct.col.ast.util.Declarator
import vct.col.ast.{Declaration, PVLConstructor}

trait PVLConstructorImpl[G] extends Declarator[G] { this: PVLConstructor[G] =>
  override def declarations: Seq[Declaration[G]] = args ++ contract.givenArgs ++ contract.yieldsArgs
}