package vct.col.ast.lang

import vct.col.ast.util.Declarator
import vct.col.ast.{Declaration, JavaConstructor}

trait JavaConstructorImpl[G] extends Declarator[G] { this: JavaConstructor[G] =>
  override def declarations: Seq[Declaration[G]] = parameters ++ typeParameters ++ contract.givenArgs ++ contract.yieldsArgs
  override def isStatic = false
}