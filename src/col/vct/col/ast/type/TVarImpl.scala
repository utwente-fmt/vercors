package vct.col.ast.`type`

import vct.col.ast.{TAnyValue, TType, TVar}
import vct.col.check.{CheckContext, CheckError, GenericTypeError}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.TVarOps

trait TVarImpl[G] extends TVarOps[G] { this: TVar[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] =
    context.checkInScope(this, ref) ++
      (if(TType(TAnyValue()).superTypeOf(ref.decl.t)) Nil
      else Seq(GenericTypeError(this, TType(TAnyValue()))))

  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(ref))
}