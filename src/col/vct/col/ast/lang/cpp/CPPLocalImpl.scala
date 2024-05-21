package vct.col.ast.lang.cpp

import vct.col.ast.{CPPLocal, CPPPrimitiveType, SYCLTAccessMode, TEnum, Type}
import vct.col.print.{Ctx, Doc, Group, Text, Empty}
import vct.col.resolve.ctx._
import vct.col.resolve.lang.CPP
import vct.col.typerules.Types
import vct.col.ast.ops.CPPLocalOps

trait CPPLocalImpl[G] extends CPPLocalOps[G] { this: CPPLocal[G] =>
  override lazy val t: Type[G] = ref.get match {
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
    case _: RefSYCLAccessMode[G] => SYCLTAccessMode()
    case ref: RefSYCLConstructorDefinition[G] => Types.notAValue(ref)
    case RefModelField(field) => field.t
    case target: SpecInvocationTarget[G] => Types.notAValue(target)
    case cls: RefClass[G] => Types.notAValue(cls)
    case enum: RefEnum[G] => Types.notAValue(enum)
    case RefEnumConstant(enum, _) => TEnum(enum.get.ref)
  }

  override def layout(implicit ctx: Ctx): Doc = Group(Text(name) <>
    (if (genericArgs.nonEmpty) (Text("<") <> Doc.args(genericArgs) <> Text(">")) else Empty))
}