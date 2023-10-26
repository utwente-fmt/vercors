package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLCommunicateSubject, TClass, Type}
import vct.col.resolve.ctx.RefPVLEndpoint

trait PVLCommunicateSubjectImpl[G] { this: PVLCommunicateSubject[G] =>
  def t: Type[G]
  def ref: Option[RefPVLEndpoint[G]]
}
