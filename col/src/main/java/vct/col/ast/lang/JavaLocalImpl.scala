package vct.col.ast.lang

import hre.util.FuncTools
import vct.col.ast._
import vct.col.ast.lang.JavaLocalImpl.IncompleteRef
import vct.col.resolve.ctx._
import vct.col.typerules.Types
import vct.result.VerificationError.SystemError

case object JavaLocalImpl {
  case class IncompleteRef(local: JavaLocal[_]) extends SystemError {
    override def text: String = local.o.messageInContext("Could not determine type of JavaLocal that was resolved as enum")
  }
}

trait JavaLocalImpl[G] { this: JavaLocal[G] =>
  override def t: Type[G] = ref.get match {
    case ref: RefAxiomaticDataType[G] => Types.notAValue(ref)
    case ref: RefEnum[G] => Types.notAValue(ref)
    case RefEnumConstant(Some(enum), _) => TEnum(enum.ref[Enum[G]])
    case RefEnumConstant(_, _) => throw IncompleteRef(this)
    case RefVariable(decl) => decl.t
    case ref: RefUnloadedJavaNamespace[G] => Types.notAValue(ref)
    case ref: RefJavaClass[G] => Types.notAValue(ref)
    case RefJavaField(decls, idx) => FuncTools.repeat[Type[G]](TArray(_), decls.decls(idx).moreDims, decls.t)
    case RefJavaLocalDeclaration(decls, idx) => FuncTools.repeat[Type[G]](TArray(_), decls.decls(idx).moreDims, decls.t)
    case RefModelField(field) => field.t
  }
}