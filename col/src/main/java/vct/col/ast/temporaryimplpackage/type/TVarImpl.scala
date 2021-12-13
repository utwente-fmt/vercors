package vct.col.ast.temporaryimplpackage.`type`

import vct.col.ast.{TAny, TType, TVar}
import vct.col.check.{CheckContext, CheckError, GenericTypeError}

trait TVarImpl[G] { this: TVar[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] =
    context.checkInScope(this, ref) ++
      (if(TType(TAny()).superTypeOf(ref.decl.t)) Nil
      else Seq(GenericTypeError(this, TType(TAny()))))
}