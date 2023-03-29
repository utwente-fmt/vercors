package vct.col.ast.lang

import vct.col.ast.{PVLDeref, TEnum, Enum, TNotAValue, Type}
import vct.col.resolve.ctx._

trait PVLDerefImpl[G] { this: PVLDeref[G] =>
  override lazy val t: Type[G] = ref.get match {
    case ref: RefModelField[G] => ref.decl.t
    case ref: RefField[G] => ref.decl.t
    case ref: RefEnumConstant[G] => obj.t match {
      case TNotAValue(RefEnum(enum)) => TEnum(enum.ref[Enum[G]])
    }
    case ref: BuiltinField[G] => ref.f(obj).t
  }
}