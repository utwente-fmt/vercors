package vct.col.ast.temporaryimplpackage.expr.context

import vct.col.ast.{AmbiguousResult, Type}
import vct.col.err
import vct.col.resolve._

trait AmbiguousResultImpl { this: AmbiguousResult =>
  override def t: Type = ref.getOrElse(
    throw err.ContextSensitiveNodeNotResolved(this, "'\\result' encountered, but its attached method is not resolved.")) match {
    case RefCFunctionDefinition(decl) =>
      C.typeOrReturnTypeFromDeclaration(decl.specs, decl.declarator)
    case RefCGlobalDeclaration(decls, initIdx) =>
      C.typeOrReturnTypeFromDeclaration(decls.decl.specs, decls.decl.inits(initIdx).decl)
    case RefFunction(decl) => decl.returnType
    case RefProcedure(decl) => decl.returnType
    case RefJavaMethod(decl) => decl.returnType
    case RefInstanceFunction(decl) => decl.returnType
    case RefInstanceMethod(decl) => decl.returnType
  }
}