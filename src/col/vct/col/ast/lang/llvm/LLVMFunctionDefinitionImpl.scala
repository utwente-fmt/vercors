package vct.col.ast.lang.llvm

import vct.col.ast.declaration.category.ApplicableImpl
import vct.col.ast.{
  Declaration,
  LLVMFunctionDefinition,
  NormalFunction,
  PallasFunctionContract,
  PredicateDefinition,
  Statement,
  Variable,
  WrapperFunction,
}
import vct.col.ast.util.Declarator
import vct.col.ast.ops.LLVMFunctionDefinitionOps
import vct.col.print._

trait LLVMFunctionDefinitionImpl[G]
    extends Declarator[G]
    with ApplicableImpl[G]
    with LLVMFunctionDefinitionOps[G] {
  this: LLVMFunctionDefinition[G] =>
  override def declarations: Seq[Declaration[G]] =
    args ++ contract.givenArgs ++ contract.yieldsArgs

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

  val isWrapper: Boolean =
    functionType match {
      case _: WrapperFunction[G] => true
      case _ => false
    }

  val isPredicate: Boolean =
    functionType match {
      case _: PredicateDefinition[G] => true
      case _ => false
    }
}
