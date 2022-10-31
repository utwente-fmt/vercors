package vct.col.ast.langspecific.cpp

import vct.col.ast.{CPPStructAccess, Type}
import vct.col.resolve._
import vct.col.util.Types

trait CPPStructAccessImpl[G] {
  this: CPPStructAccess[G] =>
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
