package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{PVLDeref, Type}
import vct.col.resolve.{BuiltinField, RefField, RefModelField}

trait PVLDerefImpl { this: PVLDeref =>
  override def t: Type = ref.get match {
    case ref: RefModelField => ref.decl.t
    case ref: RefField => ref.decl.t
    case ref: BuiltinField => ref.f(obj).t
  }
}