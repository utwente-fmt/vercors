package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{PVLLocal, TNotAValue, Type}
import vct.col.resolve.{RefAxiomaticDataType, RefClass, RefField, RefModelField, RefVariable}
import vct.col.util.Types

trait PVLLocalImpl[G] { this: PVLLocal[G] =>
  override def t: Type[G] = ref.get match {
    case ref: RefAxiomaticDataType[G] => Types.notAValue(ref)
    case ref: RefVariable[G] => ref.decl.t
    case ref: RefClass[G] => Types.notAValue(ref)
    case ref: RefField[G] => ref.decl.t
    case ref: RefModelField[G] => ref.decl.t
  }
}