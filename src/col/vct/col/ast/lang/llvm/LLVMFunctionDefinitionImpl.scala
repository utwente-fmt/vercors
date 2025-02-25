package vct.col.ast.lang.llvm

import vct.col.ast.declaration.category.ApplicableImpl
import vct.col.ast.{Declaration, LLVMFunctionDefinition, Statement}
import vct.col.ast.util.Declarator
import vct.col.ast.ops.LLVMFunctionDefinitionOps
import vct.col.print._

trait LLVMFunctionDefinitionImpl[G]
    extends Declarator[G]
    with ApplicableImpl[G]
    with LLVMFunctionDefinitionOps[G] {
  this: LLVMFunctionDefinition[G] =>
  override def declarations: Seq[Declaration[G]] = args

  override def body: Option[Statement[G]] = functionBody

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(
        (if (pure)
           Text("pure") <+> returnType
         else
           returnType.show) <+> ctx.name(this) <> "(" <> Doc.args(args) <> ")"
      ) <+> body.map(_.layoutAsBlock).getOrElse(Text("")),
    ))
}
