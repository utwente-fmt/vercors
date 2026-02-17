package vct.col.ast.expr.resource

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.{Node, Type, Unfolding}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.UnfoldingOps
import vct.col.check.{CheckContext, ResourceInPostcondition}

trait UnfoldingImpl[G] extends NodeFamilyImpl[G] with UnfoldingOps[G] {
  this: Unfolding[G] =>
  override def t: Type[G] = body.t

  def layoutPVL(implicit ctx: Ctx): Doc =
    Group(Text("unfolding") <+> res.show <+> "in" <>> body)

  def layoutSilver(implicit ctx: Ctx): Doc =
    Group(Text("unfolding") <+> res.show <+> "in" <>> body)

  def layoutJava(implicit ctx: Ctx): Doc =
    Group(Doc.inlineSpec(Text("\\Unfolding") <+> res.show <+> "\\in") <>> body)

  def layoutSpec(implicit ctx: Ctx): Doc =
    Group(Text("\\unfolding") <+> res.show <+> "\\in" <>> body)

  override def precedence: Int = Precedence.PVL_UNFOLDING
  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.PVL => layoutPVL
      case Ctx.Silver => layoutSilver
      case Ctx.Java => layoutJava
      case _ => layoutSpec
    }

  override def checkContextRecursor[T](
      context: CheckContext[G],
      f: (CheckContext[G], Node[G]) => T,
  ): Seq[T] = {
    /* in postconditions of pure functions, ResourceTerms are forbidden and throw an error.
       However, "\unfolding res() \in expr" should be allowed, so we filter out the error if the offending
       ResourceTerm is the target of this Unfolding expression.
     */
    // modified version of super.checkContextRecursor
    val res = subnodes.map(n => (n, f(enterCheckContext(context), n)))
    // filtering
    res.map {
      case (this.res, errors) =>
        errors match {
          case s: Seq[_] =>
            s.filter {
              case ResourceInPostcondition(_) => false
              case _ => true
            }.asInstanceOf[T]
          case other => other
        }
      case (_, errors) => errors
    }
  }
}
