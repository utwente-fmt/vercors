package vct.col.ast.declaration.applicableref

import vct.col.ast.{Applicable, ApplicableRef}
import vct.col.ref.Ref

trait ApplicableRefImpl[G] { this: ApplicableRef[G] =>
  def app: Ref[G, Applicable[G]]
}
