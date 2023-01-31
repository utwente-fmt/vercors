package vct.col.ast.declaration.applicableref

import vct.col.ast.{ADTFunctionRef, Applicable}
import vct.col.ref.Ref

trait ADTFunctionRefImpl[G] { this: ADTFunctionRef[G] =>
  lazy val app: Ref[G, Applicable[G]] = ref.get.ref
}
