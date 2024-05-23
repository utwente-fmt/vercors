package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.RuntimeFractionZeroOps

trait RuntimeFractionZeroImpl[G] extends RuntimeFractionZeroOps[G] {
  this: RuntimeFractionZero[G] =>

  override def t: Type[G] = TRuntimeFraction[G]()
  override def precedence: Int = Precedence.ATOMIC

  override def layout(implicit ctx: Ctx): Doc =
    Text("Fraction.ZERO")
}