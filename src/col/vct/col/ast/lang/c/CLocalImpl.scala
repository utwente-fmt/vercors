package vct.col.ast.lang.c

import vct.col.ast.ops.CLocalOps
import vct.col.ast._
import vct.col.print.{Ctx, Doc, Text}
import vct.col.resolve.ctx._
import vct.col.resolve.lang.C
import vct.col.typerules.Types

trait CLocalImpl[G] extends CLocalOps[G] {
  this: CLocal[G] =>
  override lazy val t: Type[G] =
    ref.get match {
      case ref: RefCParam[G] =>
        C.typeOrReturnTypeFromDeclaration(
          ref.decl.specifiers,
          ref.decl.declarator,
        )
      case ref: RefAxiomaticDataType[G] => Types.notAValue(ref)
      case RefVariable(decl) => decl.t
      case ref: RefCFunctionDefinition[G] => Types.notAValue(ref)
      case ref: RefCStruct[G] => Types.notAValue(ref)
      case ref @ RefCGlobalDeclaration(decls, initIdx) =>
        val declInfo = C.getDeclaratorInfo(decls.decl.inits(initIdx).decl)
        declInfo.params match {
          case Some(_) => Types.notAValue(ref) // Function declaration
          case None =>
            declInfo
              .typeOrReturnType(CPrimitiveType(
                decls.decl.specs
              )) // Static declaration
        }
      case ref @ RefCLocalDeclaration(decls, initIdx) =>
        val declInfo = C.getDeclaratorInfo(decls.decl.inits(initIdx).decl)
        declInfo.params match {
          case Some(_) => Types.notAValue(ref) // Function declaration
          case None =>
            declInfo
              .typeOrReturnType(CPrimitiveType(
                decls.decl.specs
              )) // Static declaration
        }
      case RefModelField(field) => field.t
      case target: SpecInvocationTarget[G] => Types.notAValue(target)
      case _: RefCudaVec[G] => CTCudaVec()
      case cls: RefClass[G] => Types.notAValue(cls)
      case ref: RefEnum[G] => Types.notAValue(ref)
      case RefEnumConstant(decl, _) => TEnum(decl.get.ref)
    }

  override def layout(implicit ctx: Ctx): Doc = Text(name)
}
