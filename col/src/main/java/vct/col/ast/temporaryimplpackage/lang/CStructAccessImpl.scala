package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{CStructAccess, TNotAValue, Type}
import vct.col.resolve.{BuiltinField, BuiltinInstanceMethod, RefADTFunction, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefModelAction, RefModelField, RefModelProcess, RefPredicate, RefProcedure}
import vct.col.util.Types

trait CStructAccessImpl[G] { this: CStructAccess[G] =>
  override def t: Type[G] = ref.get match {
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
  }
}