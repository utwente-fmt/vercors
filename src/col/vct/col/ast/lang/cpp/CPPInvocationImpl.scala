package vct.col.ast.lang.cpp

import vct.col.ast.{CPPInvocation, CPPTLambda, Type}
import vct.col.print._
import vct.col.resolve.ctx._
import vct.col.resolve.lang.CPP
import vct.result.VerificationError.Unreachable
import vct.col.ast.ops.CPPInvocationOps

trait CPPInvocationImpl[G] extends CPPInvocationOps[G] { this: CPPInvocation[G] =>
  override lazy val t: Type[G] = ref.get match {
    case RefFunction(decl) =>  decl.returnType
    case RefProcedure(decl) => decl.returnType
    case RefPredicate(decl) => decl.returnType
    case RefADTFunction(decl) => decl.returnType
    case RefModelProcess(decl) => decl.returnType
    case RefModelAction(decl) => decl.returnType
    case RefCPPFunctionDefinition(decl) => CPP.typeOrReturnTypeFromDeclarator(decl.specs, decl.declarator)
    case RefCPPGlobalDeclaration(decls, initIdx) => CPP.typeOrReturnTypeFromDeclarator(decls.decl.specs, decls.decl.inits(initIdx).decl)
    case RefSYCLConstructorDefinition(typ) => typ
    case RefCPPLambdaDefinition(_) => CPPTLambda()
    case RefProverFunction(decl) => decl.returnType
    case RefInstanceMethod(decl) => decl.returnType
    case RefInstanceFunction(decl) => decl.returnType
    case RefInstancePredicate(decl) => decl.returnType
    case BuiltinInstanceMethod(f) => applicable match {
      case _ => throw Unreachable("BuiltinInstanceMethod resolution of CPPInvocation cannot invoke anything.")
    }
  }

  override def precedence: Int = Precedence.POSTFIX

  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(applicable) <> "(" <> Doc.args(args) <> ")" <> DocUtil.givenYields(givenArgs, yields))
}