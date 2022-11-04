package vct.col.ast.expr.resource

import vct.col.ast.{InhaleExhaleAssertion, Type}
import vct.col.typerules.Types

trait InhaleExhaleAssertionImpl[G] { this: InhaleExhaleAssertion[G] =>
  override def t: Type[G] = Types.leastCommonSuperType(whenInhaling.t, whenExhaling.t)
}
