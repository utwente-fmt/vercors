package vct.col.ast.lang

import vct.col.ast.{CPPInvocation, Type}
import vct.col.print._
import vct.col.resolve.ctx._
import vct.col.resolve.lang.CPP
import vct.result.VerificationError.Unreachable

trait CPPInvocationImpl[G] { this: CPPInvocation[G] =>
  override lazy val t: Type[G] = ref.get match {
    case RefFunction(decl) =>  decl.returnType
    case RefProcedure(decl) => decl.returnType
    case RefPredicate(decl) => decl.returnType
    case RefADTFunction(decl) => decl.returnType
    case RefModelProcess(decl) => decl.returnType
    case RefModelAction(decl) => decl.returnType
    case RefCPPFunctionDefinition(decl) => CPP.typeOrReturnTypeFromDeclaration(decl.specs, decl.declarator)
    case RefCPPGlobalDeclaration(decls, initIdx) => CPP.typeOrReturnTypeFromDeclaration(decls.decl.specs, decls.decl.inits(initIdx).decl)
    case RefInstanceMethod(decl) => decl.returnType
    case RefInstanceFunction(decl) => decl.returnType
    case RefInstancePredicate(decl) => decl.returnType
  }

  override def precedence: Int = Precedence.POSTFIX

  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(applicable) <> "(" <> Doc.args(args) <> ")" <> DocUtil.givenYields(givenArgs, yields))
}