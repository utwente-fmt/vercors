package vct.col.ast.temporaryimplpackage.lang

import hre.util.FuncTools
import vct.col.ast.{JavaLocal, TArray, TNotAValue, Type}
import vct.col.resolve.{RefAxiomaticDataType, RefJavaClass, RefJavaField, RefJavaLocalDeclaration, RefModelField, RefUnloadedJavaNamespace, RefVariable}

trait JavaLocalImpl { this: JavaLocal =>
  override def t: Type = ref.get match {
    case ref: RefAxiomaticDataType => new TNotAValue(Some(ref))
    case RefVariable(decl) => decl.t
    case ref: RefUnloadedJavaNamespace => new TNotAValue(Some(ref))
    case ref: RefJavaClass => new TNotAValue(Some(ref))
    case RefJavaField(decls, idx) => FuncTools.repeat(TArray(_), decls.decls(idx)._2, decls.t)
    case RefJavaLocalDeclaration(decls, idx) => FuncTools.repeat(TArray(_), decls.decls(idx)._2, decls.t)
    case RefModelField(field) => field.t
  }
}