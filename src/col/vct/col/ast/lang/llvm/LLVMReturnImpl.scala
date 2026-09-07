package vct.col.ast.lang.llvm

import vct.col.ast.{
  Expr,
  LLVMFunctionDefinition,
  LLVMPredicateDefinition,
  LLVMReturn,
  Void,
}
import vct.col.ast.ops.LLVMReturnOps
import vct.col.ast.statement.exceptional.ExceptionalStatementImpl
import vct.col.check.{CheckContext, CheckError, LLVMReturnOutsideFunction}
import vct.col.print.{Ctx, Doc, Empty, Nest, Text}

trait LLVMReturnImpl[G]
    extends ExceptionalStatementImpl[G] with LLVMReturnOps[G] {
  this: LLVMReturn[G] =>

  override def check(context: CheckContext[G]): Seq[CheckError] = {
    val fDef = context.declarationStack.collectFirst {
      case _: LLVMFunctionDefinition[G] | _: LLVMPredicateDefinition[G] => ()
    }
    val wrongReturn =
      if (fDef.isEmpty)
        Seq(LLVMReturnOutsideFunction(this))
      else
        Nil
    super.check(context) ++ wrongReturn
  }

  override def layout(implicit ctx: Ctx): Doc =
    Text("return") <>
      (if (result == Void[G]())
         Text(";")
       else
         Empty <+> Nest(result.show) <> ";")

  override def expr: Expr[G] = this.result
}
