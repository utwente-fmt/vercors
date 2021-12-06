package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{PVLLocal, TNotAValue, Type}
import vct.col.resolve.{RefAxiomaticDataType, RefClass, RefField, RefModelField, RefVariable}

trait PVLLocalImpl { this: PVLLocal =>
  override def t: Type = ref.get match {
    case ref: RefAxiomaticDataType => new TNotAValue(Some(ref))
    case ref: RefVariable => ref.decl.t
    case ref: RefClass => new TNotAValue(Some(ref))
    case ref: RefField => ref.decl.t
    case ref: RefModelField => ref.decl.t
  }
}