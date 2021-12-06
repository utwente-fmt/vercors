package vct.col.ast.temporaryimplpackage.util

import vct.col.ast.temporaryimplpackage.node.NodeImpl
import vct.col.ast.{Declaration, Node}
import vct.col.check.{CheckContext, DoesNotDefine, OutOfScopeError}

trait Declarator extends NodeImpl { this: Node =>
  def declarations: Seq[Declaration]

  override def enterCheckContext(context: CheckContext): CheckContext =
    context.withScope(declarations.toSet)

  def checkDefines(defn: Declaration, use: Node): Seq[DoesNotDefine] =
    if(declarations.contains(defn)) Nil
    else Seq(DoesNotDefine(this, defn, use))
}
