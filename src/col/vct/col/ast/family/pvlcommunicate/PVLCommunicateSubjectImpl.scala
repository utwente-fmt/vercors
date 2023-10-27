package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLCommunicateSubject, TClass, Class, Type}
import vct.col.resolve.ctx.RefPVLEndpoint

trait PVLCommunicateSubjectImpl[G] { this: PVLCommunicateSubject[G] =>
  def cls: Class[G] = ref.get.decl.cls.decl
  def ref: Option[RefPVLEndpoint[G]]
}
