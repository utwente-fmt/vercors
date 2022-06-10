package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{Declaration, JavaFocus, JavaIgnore, JavaMethod, JavaStatic}
import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.origin.DiagnosticOrigin

trait JavaMethodImpl[G] extends Declarator[G] { this: JavaMethod[G] =>
  override def declarations: Seq[Declaration[G]] = parameters ++ typeParameters ++ contract.givenArgs ++ contract.yieldsArgs
  override def isStatic = modifiers.contains(JavaStatic[G]())
  def isIgnored = modifiers.contains(JavaIgnore[G]())
  def isFocused = modifiers.contains(JavaFocus[G]())
}