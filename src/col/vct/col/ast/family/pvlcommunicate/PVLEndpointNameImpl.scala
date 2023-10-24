package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLEndpointName, TClass, Type}
import vct.col.resolve.ctx.RefEndpoint

trait PVLEndpointNameImpl[G] { this: PVLEndpointName[G] =>
  override def threadType: TClass[G] = ref.get match {
    case RefEndpoint(decl) => decl.t match {
      case t @ TClass(_) => t
      case _ => ???
    }
    case _ => ???
  }
}
