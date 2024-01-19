package vct.col.ast.statement.exceptional

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Empty, Text}
import vct.col.ast.ops.ReturnOps
import vct.col.check.{CheckContext, CheckError, ReturnOutsideMethod}

trait ReturnImpl[G] extends ExceptionalStatementImpl[G] with ReturnOps[G] { this: Return[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] = {
    val app = context.declarationStack.collectFirst {
      case _: Procedure[G] | _: InstanceMethod[G] | _: InstanceOperatorMethod[G] => ()
      case _: JavaMethod[G] | _: CFunctionDefinition[G] | _: CPPFunctionDefinition[G] => ()
    }
    val wrongReturn = if(app.isEmpty) Seq(ReturnOutsideMethod(this)) else Nil
    super.check(context) ++ wrongReturn
  }

  override def layout(implicit ctx: Ctx): Doc =
    Text("return") <> (if(result == Void[G]()) Text(";") else Empty <+> result <> ";")
}