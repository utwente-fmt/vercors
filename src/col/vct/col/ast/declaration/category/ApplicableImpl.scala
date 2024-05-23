package vct.col.ast.declaration.category

import vct.col.ast.declaration.DeclarationImpl
import vct.col.ast.util.Declarator
import vct.col.ast.{Applicable, Declaration, Node, Type, Variable}
import vct.col.check.CheckContext

trait ApplicableImpl[G] extends DeclarationImpl[G] with Declarator[G] { this: Applicable[G] =>
  def args: Seq[Variable[G]]
  def returnType: Type[G]
  def body: Option[Node[G]]

  override def declarations: Seq[Declaration[G]] = args

  override def enterCheckContextCurrentApplicable(context: CheckContext[G]): Option[Applicable[G]] = Some(this)
}
