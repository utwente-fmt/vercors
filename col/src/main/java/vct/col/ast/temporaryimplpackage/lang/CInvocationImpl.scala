package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{CInvocation, CStructAccess, Type}
import vct.col.resolve.{BuiltinInstanceMethod, C, RefADTFunction, RefCDeclaration, RefCFunctionDefinition, RefCGlobalDeclaration, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefModelAction, RefModelProcess, RefPredicate, RefProcedure}
import vct.result.VerificationResult.Unreachable

trait CInvocationImpl[G] { this: CInvocation[G] =>
  override def t: Type[G] = ref.get match {
    case RefFunction(decl) =>  decl.returnType
    case RefProcedure(decl) => decl.returnType
    case RefPredicate(decl) => decl.returnType
    case RefADTFunction(decl) => decl.returnType
    case RefModelProcess(decl) => decl.returnType
    case RefModelAction(decl) => decl.returnType
    case RefCFunctionDefinition(decl) => C.typeOrReturnTypeFromDeclaration(decl.specs, decl.declarator)
    case RefCGlobalDeclaration(decls, initIdx) => C.typeOrReturnTypeFromDeclaration(decls.decl.specs, decls.decl.inits(initIdx).decl)
    case RefCDeclaration(decls, initIdx) => C.typeOrReturnTypeFromDeclaration(decls.specs, decls.inits(initIdx).decl)
    case RefInstanceMethod(decl) => decl.returnType
    case RefInstanceFunction(decl) => decl.returnType
    case RefInstancePredicate(decl) => decl.returnType
    case BuiltinInstanceMethod(f) => applicable match {
      case CStructAccess(obj, _) => f(obj)(args).t
      case other => throw Unreachable("BuiltinInstanceMethod resolution of CInvocation must invoke a CStructAccess.")
    }
  }
}