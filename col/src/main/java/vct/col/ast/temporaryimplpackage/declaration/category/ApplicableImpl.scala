package vct.col.ast.temporaryimplpackage.declaration.category

import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.ast.{Applicable, Declaration, Node, Type, Variable}
import vct.col.check.CheckContext

trait ApplicableImpl extends Declarator { this: Applicable =>
  def args: Seq[Variable]
  def returnType: Type
  def body: Option[Node]

  override def declarations: Seq[Declaration] = args

  override def enterCheckContext(context: CheckContext): CheckContext =
    super.enterCheckContext(context).withApplicable(this)
}
