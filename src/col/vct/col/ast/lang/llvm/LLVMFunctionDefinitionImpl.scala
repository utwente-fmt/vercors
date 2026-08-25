package vct.col.ast.lang.llvm

import vct.col.ast.declaration.category.ApplicableImpl
import vct.col.ast.{
  Declaration,
  GhostWrapperFunction,
  LLVMFunctionArgument,
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
      if (hasNoreturnAttr) { Text("@Noreturn") }
      else { Empty },
      if (isWrapper) { Text("@Wrapper") }
      else if (isGhostWrapper) { Text("@GhostWrapper") }
      else if (isPredicate) { Text("@Predicate") }
      else { contract },
      Group(
        (if (pure)
           Text("pure") <+> returnType
         else
           returnType.show) <+> ctx.name(this) <> "(" <> Doc.args(llvmArgs) <>
          ")"
      ) <+> body.map(_.layoutAsBlock).getOrElse(Text("")),
    ))

  val args: Seq[Variable[G]] = llvmArgs.map(_.v)

  // All arguments that have a byval-attibute
  val byValArgs: Seq[LLVMFunctionArgument[G]] = llvmArgs
    .filter(_.byValType.nonEmpty)

  // Sret-argument if one exists
  val sretArg: Option[LLVMFunctionArgument[G]] =
    llvmArgs.filter(_.isSret).headOption

  // All arguments that do NOT have the sret-attribute
  val argsWithoutSret: Seq[LLVMFunctionArgument[G]] = {
    sretArg match {
      case Some(retArg) => llvmArgs.filter(a => a != retArg)
      case None => llvmArgs
    }
  }

  val isWrapper: Boolean =
    functionType match {
      case _: WrapperFunction[G] => true
      case _ => false
    }

  val isGhostWrapper: Boolean =
    functionType match {
      case _: GhostWrapperFunction[G] => true
      case _ => false
    }

  val isPredicate: Boolean =
    functionType match {
      case _: PredicateDefinition[G] => true
      case _ => false
    }
}
