package vct.col.ast.expr.context

import vct.col.ast.{Constructor, TClass, ThisObject, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.ThisObjectOps
import vct.col.check.{CheckContext, CheckError, ThisInConstructorPre}

trait ThisObjectImpl[G] extends ThisDeclarationImpl[G] with ThisObjectOps[G] { this: ThisObject[G] =>
  override def t: Type[G] = TClass(cls)

  override def check(context: CheckContext[G]): Seq[CheckError] = {
    val inConstructor = context.declarationStack.collectFirst { case _: Constructor[G] => () }.nonEmpty
    super.check(context) ++ (if(inConstructor && context.inPreCondition) Seq(ThisInConstructorPre(this)) else Nil)
  }

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("this")
}