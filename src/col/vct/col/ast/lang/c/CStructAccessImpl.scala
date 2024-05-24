package vct.col.ast.lang.c

import vct.col.ast.{CStructAccess, TEnum, TCInt, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.resolve.ctx._
import vct.col.typerules.Types
import vct.col.ast.ops.CStructAccessOps
import vct.col.resolve.lang.C

trait CStructAccessImpl[G] extends CStructAccessOps[G] { this: CStructAccess[G] =>
  override lazy val t: Type[G] = ref.get match {
    case ref: RefCStructField[G] => C.typeOrReturnTypeFromDeclaration(ref.decls.specs, ref.decls.decls(ref.idx))
    case ref: RefModelField[G] => ref.decl.t
    case ref: RefFunction[G] => Types.notAValue(ref)
    case ref: RefProcedure[G] => Types.notAValue(ref)
    case ref: RefPredicate[G] => Types.notAValue(ref)
    case ref: RefInstanceFunction[G] => Types.notAValue(ref)
    case ref: RefInstanceMethod[G] => Types.notAValue(ref)
    case ref: RefInstancePredicate[G] => Types.notAValue(ref)
    case ref: RefADTFunction[G] => Types.notAValue(ref)
    case ref: RefModelProcess[G] => Types.notAValue(ref)
    case ref: RefModelAction[G] => Types.notAValue(ref)
    case ref: BuiltinField[G] => ref.f(struct).t
    case ref: BuiltinInstanceMethod[G] => Types.notAValue(ref)
    case ref: RefCudaVecDim[G] => TCInt()
    case RefEnumConstant(enum, _) => TEnum(enum.get.ref)
    case RefProverFunction(decl) => decl.returnType
  }

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    assoc(struct) <> "." <> field
}