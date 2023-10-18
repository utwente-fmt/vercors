package vct.col.ast.lang

import vct.col.ast.{GpgpuCudaKernelInvocation, Type}
import vct.col.print._
import vct.col.resolve.ctx._
import vct.col.resolve.lang.C

trait GpgpuCudaKernelInvocationImpl[G] {
  this: GpgpuCudaKernelInvocation[G] =>
  override lazy val t: Type[G] =
    ref.get match {
      case RefFunction(decl) => decl.returnType
      case RefProcedure(decl) => decl.returnType
      case RefPredicate(decl) => decl.returnType
      case RefInstanceFunction(decl) => decl.returnType
      case RefInstanceMethod(decl) => decl.returnType
      case RefInstancePredicate(decl) => decl.returnType
      case RefADTFunction(decl) => decl.returnType
      case RefModelProcess(decl) => decl.returnType
      case RefModelAction(decl) => decl.returnType
      case RefCFunctionDefinition(decl) =>
        C.typeOrReturnTypeFromDeclaration(decl.specs, decl.declarator)
      case RefCGlobalDeclaration(decls, initIdx) =>
        C.typeOrReturnTypeFromDeclaration(
          decls.decl.specs,
          decls.decl.inits(initIdx).decl,
        )
      case BuiltinInstanceMethod(f) => ???
      case RefLlvmSpecFunction(decl) => decl.returnType
      case RefProverFunction(decl) => decl.returnType
    }

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Group(
        Group(
          Text(kernel) <> "<<<" <> Doc.args(Seq(blocks, threads)) <> ">>>"
        ) <> "(" <> Doc.args(args) <> ")"
      ) <> DocUtil.givenYields(givenArgs, yields)
    )
}
