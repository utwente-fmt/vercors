package vct.col.ast.lang

import vct.col.ast.{CPPClassInstanceLocal, CPPLocal, CPPPrimitiveType, Type}
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.resolve.ctx._
import vct.col.resolve.lang.CPP
import vct.col.typerules.Types

trait CPPClassInstanceLocalImpl[G] { this: CPPClassInstanceLocal[G] =>
  // Keep same as CPPLocalImpl
  override lazy val t: Type[G] = classLocalRef.get match {
    case ref: RefCPPParam[G] => CPP.typeOrReturnTypeFromDeclarator(ref.decl.specifiers, ref.decl.declarator)
    case ref: RefAxiomaticDataType[G] => Types.notAValue(ref)
    case RefVariable(decl) => decl.t
    case ref: RefCPPFunctionDefinition[G] => Types.notAValue(ref)
    case ref @ RefCPPGlobalDeclaration(decls, initIdx) =>
      val declInfo = CPP.getDeclaratorInfo(decls.decl.inits(initIdx).decl)
      declInfo.params match {
        case Some(_) => Types.notAValue(ref) // Function declaration
        case None => declInfo.typeOrReturnType(CPPPrimitiveType(decls.decl.specs)) // Static declaration
      }
    case ref @ RefCPPLocalDeclaration(decls, initIdx) =>
      val declInfo = CPP.getDeclaratorInfo(decls.decl.inits(initIdx).decl)
      declInfo.params match {
        case Some(_) => Types.notAValue(ref) // Function declaration
        case None => declInfo.typeOrReturnType(CPPPrimitiveType(decls.decl.specs)) // Static declaration
      }
    case RefModelField(field) => field.t
    case target: SpecInvocationTarget[G] => Types.notAValue(target)
  }

  override def layout(implicit ctx: Ctx): Doc = Group(Text(classInstanceRefName) <> "." <> Text(classLocalName))
}