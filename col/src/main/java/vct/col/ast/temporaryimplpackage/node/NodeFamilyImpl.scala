package vct.col.ast.temporaryimplpackage.node

import vct.col.ast.NodeFamily
import vct.col.check._
import vct.col.coerce.{CoercingRewriter, NopCoercingRewriter}
import vct.col.rewrite.InitialGeneration

/*
  Marker trait to indicate this node, or this hierarchy of nodes, always rewrites to itself. This is for example for
  Expr (which always rewrites to an Expr), but also single-purpose nodes, such as a catch clause.
 */
trait NodeFamilyImpl[G] extends NodeImpl[G] { this: NodeFamily[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] =
    try {
      NopCoercingRewriter().coerceAny(this.asInstanceOf[NodeFamily[InitialGeneration]])
      Nil
    } catch {
      case CoercingRewriter.Incoercible(e, t) => Seq(TypeError(e, t))
      case CoercingRewriter.IncoercibleText(e, m) => Seq(TypeErrorText(e, _ => m))
    }
}
