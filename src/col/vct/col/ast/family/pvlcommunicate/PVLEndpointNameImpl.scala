package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLEndpointName, TClass, Type}
import vct.col.resolve.ctx.RefEndpoint

trait PVLEndpointNameImpl[G] { this: PVLEndpointName[G] =>
  def t: Type[G] = ref.get.decl.t
}
