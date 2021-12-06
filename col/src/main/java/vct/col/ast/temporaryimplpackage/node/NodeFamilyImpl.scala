package vct.col.ast.temporaryimplpackage.node

import vct.col.ast.NodeFamily
import vct.col.check._
import vct.col.coerce.{CoercingRewriter, NopCoercingRewriter}

/*
  Marker trait to indicate this node, or this hierarchy of nodes, always rewrites to itself. This is for example for
  Expr (which always rewrites to an Expr), but also single-purpose nodes, such as a catch clause.
 */
trait NodeFamilyImpl extends NodeImpl { this: NodeFamily =>
  override def check(context: CheckContext): Seq[CheckError] =
    try {
      NopCoercingRewriter.coerceAny(this)
      Nil
    } catch {
      case CoercingRewriter.Incoercible(e, t) => Seq(TypeError(e, t))
      case CoercingRewriter.IncoercibleText(e, m) => Seq(TypeErrorText(e, _ => m))
    }
}
