package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.ast.{Declaration, PVLConstructor}

trait PVLConstructorImpl extends Declarator { this: PVLConstructor =>
  override def declarations: Seq[Declaration] = args ++ contract.givenArgs ++ contract.yieldsArgs
}