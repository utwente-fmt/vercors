package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{CStructAccess, TNotAValue, Type}
import vct.col.resolve.{BuiltinField, BuiltinInstanceMethod, RefADTFunction, RefFunction, RefInstanceFunction, RefInstanceMethod, RefInstancePredicate, RefModelAction, RefModelField, RefModelProcess, RefPredicate, RefProcedure}

trait CStructAccessImpl { this: CStructAccess =>
  override def t: Type = ref.get match {
    case ref: RefModelField => ref.decl.t
    case ref: RefFunction => new TNotAValue(Some(ref))
    case ref: RefProcedure => new TNotAValue(Some(ref))
    case ref: RefPredicate => new TNotAValue(Some(ref))
    case ref: RefInstanceFunction => new TNotAValue(Some(ref))
    case ref: RefInstanceMethod => new TNotAValue(Some(ref))
    case ref: RefInstancePredicate => new TNotAValue(Some(ref))
    case ref: RefADTFunction => new TNotAValue(Some(ref))
    case ref: RefModelProcess => new TNotAValue(Some(ref))
    case ref: RefModelAction => new TNotAValue(Some(ref))
    case ref: BuiltinField => ref.f(struct).t
    case ref: BuiltinInstanceMethod => new TNotAValue(Some(ref))
  }
}