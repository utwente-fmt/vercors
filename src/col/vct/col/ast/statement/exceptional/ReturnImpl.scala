package vct.col.ast.statement.exceptional

import vct.col.ast.{AbstractMethod, Constructor, Return, Void}
import vct.col.print.{Ctx, Doc, Empty, Text}
import vct.col.ast.ops.ReturnOps
import vct.col.check.{CheckContext, CheckError, ReturnOutsideMethod}

trait ReturnImpl[G] extends ExceptionalStatementImpl[G] with ReturnOps[G] { this: Return[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++ (context.currentApplicable match {
      case None => Seq(ReturnOutsideMethod(this))
      case Some(_: Constructor[G]) => Seq(ReturnOutsideMethod(this))
      case Some(_: AbstractMethod[G]) => Nil
      case Some(_) => Seq(ReturnOutsideMethod(this))
    })

  override def layout(implicit ctx: Ctx): Doc =
    Text("return") <> (if(result == Void[G]()) Text(";") else Empty <+> result <> ";")
}