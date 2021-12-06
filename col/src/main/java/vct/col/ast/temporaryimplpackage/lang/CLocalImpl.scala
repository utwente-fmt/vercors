package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{CLocal, CPrimitiveType, TNotAValue, Type}
import vct.col.resolve.{C, RefAxiomaticDataType, RefCDeclaration, RefCFunctionDefinition, RefCGlobalDeclaration, RefCParam, RefModelField, RefVariable}

trait CLocalImpl { this: CLocal =>
  override def t: Type = ref.get match {
    case ref: RefCParam => C.typeOrReturnTypeFromDeclaration(ref.decl.specifiers, ref.decl.declarator)
    case ref: RefAxiomaticDataType => new TNotAValue(Some(ref))
    case RefVariable(decl) => decl.t
    case ref: RefCFunctionDefinition => new TNotAValue(Some(ref))
    case ref @ RefCGlobalDeclaration(decls, initIdx) =>
      val declInfo = C.getDeclaratorInfo(decls.decl.inits(initIdx).decl)
      declInfo.params match {
        case Some(_) => new TNotAValue(Some(ref)) // Function declaration
        case None => declInfo.typeOrReturnType(CPrimitiveType(decls.decl.specs)) // Static declaration
      }
    case ref @ RefCDeclaration(decls, initIdx) =>
      val declInfo = C.getDeclaratorInfo(decls.inits(initIdx).decl)
      declInfo.params match {
        case Some(_) => new TNotAValue(Some(ref)) // Function declaration
        case None => declInfo.typeOrReturnType(CPrimitiveType(decls.specs)) // Static declaration
      }
    case RefModelField(field) => field.t
  }
}