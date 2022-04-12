package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{Declaration, JavaMethod, JavaStatic}
import vct.col.ast.temporaryimplpackage.util.Declarator

trait JavaMethodImpl[G] extends Declarator[G] { this: JavaMethod[G] =>
  override def declarations: Seq[Declaration[G]] = parameters ++ typeParameters ++ contract.givenArgs ++ contract.yieldsArgs
  override def isStatic = modifiers.contains(JavaStatic[G]())
}