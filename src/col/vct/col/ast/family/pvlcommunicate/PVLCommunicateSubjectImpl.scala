package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLCommunicateSubject, TClass, Class, Type}
import vct.col.resolve.ctx.RefPVLEndpoint
import vct.col.ast.ops.PVLCommunicateSubjectFamilyOps

trait PVLCommunicateSubjectImpl[G] extends PVLCommunicateSubjectFamilyOps[G] { this: PVLCommunicateSubject[G] =>
  def cls: Class[G] = ref.get.decl.cls.decl
  def ref: Option[RefPVLEndpoint[G]]
}
