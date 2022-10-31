package vct.col.ast.langspecific.cpp

import vct.col.ast.{CPPInvocation, CPPStructAccess, Type}
import vct.col.resolve._
import vct.result.VerificationError.Unreachable

trait CPPInvocationImpl[G] {
  this: CPPInvocation[G] =>


  override def t: Type[G] = ref.get match {
    case RefFunction(decl) => decl.returnType
    case RefProcedure(decl) => decl.returnType
    case RefPredicate(decl) => decl.returnType
    case RefADTFunction(decl) => decl.returnType
    case RefModelProcess(decl) => decl.returnType
    case RefModelAction(decl) => decl.returnType
    case RefCPPFunctionDefinition(decl) => CPP.typeOrReturnTypeFromDeclaration(decl.specs, decl.declarator)
    case RefCPPGlobalDeclaration(decls, initIdx) => CPP.typeOrReturnTypeFromDeclaration(decls.decl.specs, decls.decl.inits(initIdx).decl)
    case RefCPPDeclaration(decls, initIdx) => CPP.typeOrReturnTypeFromDeclaration(decls.specs, decls.inits(initIdx).decl)
    case RefInstanceMethod(decl) => decl.returnType
    case RefInstanceFunction(decl) => decl.returnType
    case RefInstancePredicate(decl) => decl.returnType
    case BuiltinInstanceMethod(f) => applicable match {
      case CPPStructAccess(obj, _) => f(obj)(args).t
      case other => throw Unreachable("BuiltinInstanceMethod resolution of CInvocation must invoke a CStructAccess.")
    }
  }


}
