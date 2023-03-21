package vct.col.ast.declaration.category

import vct.col.ast.util.Declarator
import vct.col.ast.{Applicable, Declaration, Node, Type, Variable}
import vct.col.check.CheckContext

trait ApplicableImpl[G] extends Declarator[G] { this: Applicable[G] =>
  def args: Seq[Variable[G]]
  def returnType: Type[G]
  def body: Option[Node[G]]

  override def declarations: Seq[Declaration[G]] = args

  override def enterCheckContext(context: CheckContext[G]): CheckContext[G] =
    super.enterCheckContext(context).withApplicable(this)
}
