package vct.col.ast.temporaryimplpackage.util

import vct.col.ast.{Declaration, Node}
import vct.col.check.CheckContext

trait Declarator { this: Node =>
  def declarations: Seq[Declaration]

  override def enterCheckContext(context: CheckContext): CheckContext =
    context.withScope(declarations.toSet)
}
