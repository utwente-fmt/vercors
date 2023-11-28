package vct.col.ast.lang.c

import vct.col.ast.ops.CInvocationOps
import vct.col.ast.{CInvocation, CStructAccess, Type}
import vct.col.print._
import vct.col.resolve.ctx._
import vct.col.resolve.lang.C
import vct.result.VerificationError.Unreachable

trait CInvocationImpl[G] extends CInvocationOps[G] { this: CInvocation[G] =>
  override lazy val t: Type[G] = ref.get match {
    case RefFunction(decl) =>  decl.returnType
    case RefProcedure(decl) => decl.returnType
    case RefPredicate(decl) => decl.returnType
    case RefADTFunction(decl) => decl.returnType
    case RefModelProcess(decl) => decl.returnType
    case RefModelAction(decl) => decl.returnType
    case RefCFunctionDefinition(decl) => C.typeOrReturnTypeFromDeclaration(decl.specs, decl.declarator)
    case RefCGlobalDeclaration(decls, initIdx) => C.typeOrReturnTypeFromDeclaration(decls.decl.specs, decls.decl.inits(initIdx).decl)
    case RefProverFunction(decl) => decl.returnType
    case RefInstanceMethod(decl) => decl.returnType
    case RefInstanceFunction(decl) => decl.returnType
    case RefInstancePredicate(decl) => decl.returnType
    case BuiltinInstanceMethod(f) => applicable match {
      case CStructAccess(obj, _) => f(obj)(args).t
      case other => throw Unreachable("BuiltinInstanceMethod resolution of CInvocation must invoke a CStructAccess.")
    }
  }

  override def precedence: Int = Precedence.POSTFIX

  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(applicable) <> "(" <> Doc.args(args) <> ")" <> DocUtil.givenYields(givenArgs, yields))
}