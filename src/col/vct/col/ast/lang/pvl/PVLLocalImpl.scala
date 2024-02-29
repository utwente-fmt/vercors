package vct.col.ast.lang.pvl

import vct.col.ast.{PVLLocal, TClass, TEnum, TNotAValue, Type}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.resolve.ctx._
import vct.col.typerules.Types
import vct.col.ast.ops.PVLLocalOps

trait PVLLocalImpl[G] extends PVLLocalOps[G] { this: PVLLocal[G] =>
  override lazy val t: Type[G] = ref.get match {
    case ref: RefAxiomaticDataType[G] => Types.notAValue(ref)
    case ref: RefEnum[G] => Types.notAValue(ref)
    case ref: RefVariable[G] => ref.decl.t
    case ref: RefClass[G] => Types.notAValue(ref)
    case ref: RefField[G] => ref.decl.t
    case ref: RefModelField[G] => ref.decl.t
    case ref: RefEndpoint[G] => ref.decl.t
    case ref: RefPVLEndpoint[G] => TClass[G](ref.decl.cls.decl.ref, Seq())
    case RefEnumConstant(enum, _) => TEnum(enum.get.ref)
  }

  override def layout(implicit ctx: Ctx): Doc = Text(name)
}