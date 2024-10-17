package vct.col.ast.expr.heap.alloc

import vct.col.ast.expr.ExprImpl
import vct.col.ast.{NewObject, TClass, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.NewObjectOps
import vct.col.check.{CheckContext, CheckError, TypeErrorExplanation}

trait NewObjectImpl[G] extends NewObjectOps[G] with ExprImpl[G] {
  this: NewObject[G] =>
  override def t: Type[G] = TClass(cls, Seq())

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> ctx.name(cls) <> "()"

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++
      (if (cls.decl.typeArgs.nonEmpty)
         Seq(TypeErrorExplanation(
           this,
           "This expression only supports non-generic classes",
         ))
       else
         Seq())
}
