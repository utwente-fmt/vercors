package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.ast.{Declaration, JavaConstructor}

trait JavaConstructorImpl extends Declarator { this: JavaConstructor =>
  override def declarations: Seq[Declaration] = parameters ++ typeParameters ++ contract.givenArgs ++ contract.yieldsArgs
}