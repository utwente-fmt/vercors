package vct.col.ast.temporaryimplpackage.declaration.adt

import vct.col.ast.{ADTAxiom, TBool}
import vct.col.check.{CheckContext, CheckError}

trait ADTAxiomImpl { this: ADTAxiom =>
  override def check(context: CheckContext): Seq[CheckError] = axiom.checkSubType(TBool())
}