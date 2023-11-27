package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLSubject, TClass, Class, Type}
import vct.col.resolve.ctx.RefPVLEndpoint

trait PVLSubjectImpl[G] { this: PVLSubject[G] =>
  def cls: Class[G] = ref.get.decl.cls.decl
  def ref: Option[RefPVLEndpoint[G]]
}
