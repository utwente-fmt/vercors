package vct.col.ast.temporaryimplpackage.lang

import hre.util.FuncTools
import vct.col.ast.{JavaLocal, TArray, TNotAValue, Type}
import vct.col.resolve.{RefAxiomaticDataType, RefJavaClass, RefJavaField, RefJavaLocalDeclaration, RefModelField, RefUnloadedJavaNamespace, RefVariable}
import vct.col.util.Types

trait JavaLocalImpl[G] { this: JavaLocal[G] =>
  override def t: Type[G] = ref.get match {
    case ref: RefAxiomaticDataType[G] => Types.notAValue(ref)
    case RefVariable(decl) => decl.t
    case ref: RefUnloadedJavaNamespace[G] => Types.notAValue(ref)
    case ref: RefJavaClass[G] => Types.notAValue(ref)
    case RefJavaField(decls, idx) => FuncTools.repeat[Type[G]](TArray(_), decls.decls(idx).moreDims, decls.t)
    case RefJavaLocalDeclaration(decls, idx) => FuncTools.repeat[Type[G]](TArray(_), decls.decls(idx).moreDims, decls.t)
    case RefModelField(field) => field.t
  }
}