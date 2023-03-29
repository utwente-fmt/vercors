package vct.col.ast.expr.resource

import vct.col.ast.{PolarityDependent, Type}
import vct.col.typerules.Types

trait PolarityDependentImpl[G] { this: PolarityDependent[G] =>
  override lazy val t: Type[G] = Types.leastCommonSuperType(onInhale.t, onExhale.t)
}
