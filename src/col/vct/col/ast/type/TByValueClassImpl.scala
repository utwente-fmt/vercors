package vct.col.ast.`type`

import vct.col.ast.{ByValueClass, InstanceField, TByValueClass}
import vct.col.ast.ops.TByValueClassOps
import vct.col.typerules.TypeSize

trait TByValueClassImpl[G] extends TByValueClassOps[G] {
  this: TByValueClass[G] =>

  override def bits: TypeSize = {
    val sizes = cls.decl.decls.collect { case field: InstanceField[G] =>
      field.t.bits
    }
    if (cls.decl.asInstanceOf[ByValueClass[G]].packed) {
      TypeSize.packed(sizes: _*)
    } else { TypeSize.struct(sizes: _*) }
  }
}
