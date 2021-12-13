package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.ast.{Declaration, JavaConstructor}

trait JavaConstructorImpl[G] extends Declarator[G] { this: JavaConstructor[G] =>
  override def declarations: Seq[Declaration[G]] = parameters ++ typeParameters ++ contract.givenArgs ++ contract.yieldsArgs
}