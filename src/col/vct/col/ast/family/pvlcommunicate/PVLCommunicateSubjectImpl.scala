package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLCommunicateSubject, TClass, Type}

trait PVLCommunicateSubjectImpl[G] { this: PVLCommunicateSubject[G] =>
  def threadType: TClass[G]
}
