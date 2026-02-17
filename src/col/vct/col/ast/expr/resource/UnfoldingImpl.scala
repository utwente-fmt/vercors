package vct.col.ast.expr.resource

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.{
  AmbiguousFoldTarget,
  Node,
  ScaledPredicateApply,
  Type,
  Unfolding,
}
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
       ToDo: This is a quick-fix, checking that the offending term is exactly the target; it does not work if the
        target is e.g. a scale expression. For a more generic solution, we might need e.g. a flag in the context to
        indicate "we're in an unfolding target, ResourceTerms are allowed now"
     */
    val res = super.checkContextRecursor(context, f)
    res.map {
      case s: Seq[_] =>
        s.filter {
          case ResourceInPostcondition(n) =>
            this.res match {
              case AmbiguousFoldTarget(t) if t == n => false
              case _ => true
            }
          case _ => true
        }.asInstanceOf[T]
      case other => other
    }
  }
}
