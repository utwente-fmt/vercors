package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLSubject, TClass, Class, Type}
import vct.col.resolve.ctx.RefPVLEndpoint
import vct.col.ast.ops.PVLSubjectFamilyOps

trait PVLSubjectImpl[G] extends PVLSubjectFamilyOps[G] { this: PVLSubject[G] =>
  def cls: Class[G] = ref.get.decl.cls.decl
  def ref: Option[RefPVLEndpoint[G]]
}
