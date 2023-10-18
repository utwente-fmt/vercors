package vct.col.ast.`type`

import vct.col.ast.{TAny, TType, TVar}
import vct.col.check.{CheckContext, CheckError, GenericTypeError}
import vct.col.print.{Ctx, Doc, Text}

trait TVarImpl[G] {
  this: TVar[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] =
    context.checkInScope(this, ref) ++
      (if (TType(TAny()).superTypeOf(ref.decl.t))
         Nil
       else
         Seq(GenericTypeError(this, TType(TAny()))))

  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(ref))
}
