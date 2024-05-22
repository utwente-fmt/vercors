package vct.col.ast.util

import vct.col.ast.node.NodeImpl
import vct.col.ast.{Declaration, Node}
import vct.col.check.{CheckContext, DoesNotDefine, OutOfScopeError}

trait Declarator[G] extends NodeImpl[G] { this: Node[G] =>
  def declarations: Seq[Declaration[G]]

  override def enterCheckContextScopes(context: CheckContext[G]): Seq[CheckContext.ScopeFrame[G]] =
    context.withScope(declarations)

  def checkDefines(defn: Declaration[G], use: Node[G]): Seq[DoesNotDefine] =
    if(declarations.contains(defn)) Nil
    else Seq(DoesNotDefine(this, defn, use))
}
