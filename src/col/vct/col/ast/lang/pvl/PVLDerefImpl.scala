package vct.col.ast.lang.pvl

import vct.col.ast.{TEnum, Enum, PVLDeref, TNotAValue, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.resolve.ctx._
import vct.col.ast.ops.PVLDerefOps

trait PVLDerefImpl[G] extends PVLDerefOps[G] { this: PVLDeref[G] =>
  override lazy val t: Type[G] = ref.get match {
    case ref: RefModelField[G] => ref.decl.t
    case ref: RefField[G] => ref.decl.t
    case RefEnumConstant(enum, _) => TEnum(enum.get.ref)
    case ref: BuiltinField[G] => ref.f(obj).t
  }

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    assoc(obj) <> "." <> field
}