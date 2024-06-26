package vct.col.util

import vct.col.ast.{
  Declaration,
  SuccessorsProviderNothing,
  SuccessorsProviderTrafo,
}
import vct.result.VerificationError.Unreachable

class IdentitySuccessorsProvider[G]
    extends SuccessorsProviderTrafo[G, G](new SuccessorsProviderNothing(
      throw Unreachable("Always preTransformed")
    )) {
  override def preTransform[I <: Declaration[G], O <: Declaration[G]](
      pre: I
  ): Option[O] = Some(pre.asInstanceOf[O])
}
