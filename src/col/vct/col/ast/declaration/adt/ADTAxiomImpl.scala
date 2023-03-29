package vct.col.ast.declaration.adt

import vct.col.ast.{ADTAxiom, TBool}
import vct.col.check.{CheckContext, CheckError}

trait ADTAxiomImpl[G] { this: ADTAxiom[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] = axiom.checkSubType(TBool())
}