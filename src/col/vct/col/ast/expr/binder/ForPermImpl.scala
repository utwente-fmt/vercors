package vct.col.ast.expr.binder

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.{ForPerm, TBool}
import vct.col.print._
import vct.col.ast.ops.ForPermOps
import vct.col.check.{CheckContext, CheckError, MustBeInPolarityDependent}

trait ForPermImpl[G] extends NodeFamilyImpl[G] with ForPermOps[G] {
  this: ForPerm[G] =>
  override def t: TBool[G] = TBool()

  def layoutSpec(implicit ctx: Ctx): Doc =
    Group(
      Text("(\\forperm") <+> Doc.fold(bindings)(_ <> "," <+> _) <+> "\\in" <+>
        loc <> ";" <>> body.show </> ")"
    )

  // PB: does not need parentheses, so perhaps make precedence depend on ctx
  def layoutSilver(implicit ctx: Ctx): Doc =
    Group(
      Text("(forperm") <+> Doc.fold(bindings)(_ <> "," <+> _) <+> "[" <> loc <>
        "]" <+> "::" <>> body.show </> ")"
    )

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case _ => layoutSpec
    }

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++
      (if (
         context.inPolarExpression ||
         (!context.inPreCondition && !context.inPostCondition)
       ) { Nil }
       else { Seq(MustBeInPolarityDependent(this)) })
}
