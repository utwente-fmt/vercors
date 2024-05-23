package vct.col.ast.lang.cpp

import vct.col.ast.{CPPClassMethodOrFieldAccess, TEnum, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.resolve.ctx._
import vct.col.typerules.Types
import vct.col.ast.ops.CPPClassMethodOrFieldAccessOps

trait CPPClassMethodOrFieldAccessImpl[G]
    extends CPPClassMethodOrFieldAccessOps[G] {
  this: CPPClassMethodOrFieldAccess[G] =>
  override lazy val t: Type[G] =
    ref.get match {
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
      case RefProverFunction(decl) => decl.returnType
      case ref: BuiltinField[G] => ref.f(classInstance).t
      case ref: BuiltinInstanceMethod[G] => Types.notAValue(ref)
      case RefEnumConstant(enum, _) => TEnum(enum.get.ref)
      case ref: RefCPPGlobalDeclaration[G] => Types.notAValue(ref)
    }

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    assoc(classInstance) <> "." <> methodOrFieldName
}
