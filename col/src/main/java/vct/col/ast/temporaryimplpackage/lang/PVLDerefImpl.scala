package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{PVLDeref, Type}
import vct.col.resolve.{BuiltinField, RefField, RefModelField}

trait PVLDerefImpl[G] { this: PVLDeref[G] =>
  override def t: Type[G] = ref.get match {
    case ref: RefModelField[G] => ref.decl.t
    case ref: RefField[G] => ref.decl.t
    case ref: BuiltinField[G] => ref.f(obj).t
  }
}