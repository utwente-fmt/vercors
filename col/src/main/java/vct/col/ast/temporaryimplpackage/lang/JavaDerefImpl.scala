package vct.col.ast.temporaryimplpackage.lang

import hre.util.FuncTools
import vct.col.ast.{JavaDeref, TArray, TNotAValue, Type}
import vct.col.resolve.{BuiltinField, RefAxiomaticDataType, RefJavaClass, RefJavaField, RefModel, RefModelField, RefUnloadedJavaNamespace, RefVariable}

trait JavaDerefImpl { this: JavaDeref =>
  override def t: Type = ref.get match {
    case RefModelField(decl) => decl.t
    case ref: RefUnloadedJavaNamespace => new TNotAValue(Some(ref))
    case ref: RefJavaClass => new TNotAValue(Some(ref))
    case ref: RefAxiomaticDataType => new TNotAValue(Some(ref))
    case ref: RefModel => new TNotAValue(Some(ref))
    case RefVariable(v) => v.t
    case RefJavaField(decls, idx) => FuncTools.repeat(TArray(_), decls.decls(idx)._2, decls.t)
    case BuiltinField(f) => f(obj).t
  }
}