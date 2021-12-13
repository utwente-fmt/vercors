package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.ast.{Declaration, PVLConstructor}

trait PVLConstructorImpl[G] extends Declarator[G] { this: PVLConstructor[G] =>
  override def declarations: Seq[Declaration[G]] = args ++ contract.givenArgs ++ contract.yieldsArgs
}