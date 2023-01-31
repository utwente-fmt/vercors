package vct.col.ast.declaration.applicableref

import vct.col.ast.{Applicable, FunctionRef}
import vct.col.ref.Ref

trait FunctionRefImpl[G] { this: FunctionRef[G] =>
  lazy val app: Ref[G, Applicable[G]] = ref.get.ref
}
