package vct.col.ast.expr.resource

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.{CurPerm, TRational, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.CurPermOps
import vct.col.check.{CheckContext, CheckError, MustBeInPolarityDependent}

trait CurPermImpl[G] extends NodeFamilyImpl[G] with CurPermOps[G] {
  this: CurPerm[G] =>
  override def t: Type[G] = TRational()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("perm(") <> Doc.arg(loc) <> ")")

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++
      (if (
         context.inPolarExpression ||
         (!context.inPreCondition && !context.inPostCondition)
       ) { Nil }
       else { Seq(MustBeInPolarityDependent(this)) })
}
