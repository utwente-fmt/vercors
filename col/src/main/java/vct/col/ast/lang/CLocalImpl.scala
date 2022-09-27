package vct.col.ast.lang

import vct.col.ast.{CLocal, CPrimitiveType, CTCudaVec, Type}
import vct.col.resolve.{C, RefAxiomaticDataType, RefCFunctionDefinition, RefCGlobalDeclaration, RefCLocalDeclaration, RefCParam, RefCudaVec, RefModelField, RefVariable, SpecInvocationTarget}
import vct.col.util.Types

trait CLocalImpl[G] { this: CLocal[G] =>
  override def t: Type[G] = ref.get match {
    case ref: RefCParam[G] => C.typeOrReturnTypeFromDeclaration(ref.decl.specifiers, ref.decl.declarator)
    case ref: RefAxiomaticDataType[G] => Types.notAValue(ref)
    case RefVariable(decl) => decl.t
    case ref: RefCFunctionDefinition[G] => Types.notAValue(ref)
    case ref @ RefCGlobalDeclaration(decls, initIdx) =>
      val declInfo = C.getDeclaratorInfo(decls.decl.inits(initIdx).decl)
      declInfo.params match {
        case Some(_) => Types.notAValue(ref) // Function declaration
        case None => declInfo.typeOrReturnType(CPrimitiveType(decls.decl.specs)) // Static declaration
      }
    case ref @ RefCLocalDeclaration(decls, initIdx) =>
      val declInfo = C.getDeclaratorInfo(decls.decl.inits(initIdx).decl)
      declInfo.params match {
        case Some(_) => Types.notAValue(ref) // Function declaration
        case None => declInfo.typeOrReturnType(CPrimitiveType(decls.decl.specs)) // Static declaration
      }
    case RefModelField(field) => field.t
    case target: SpecInvocationTarget[G] => Types.notAValue(target)
    case _: RefCudaVec[G] => CTCudaVec()
  }
}