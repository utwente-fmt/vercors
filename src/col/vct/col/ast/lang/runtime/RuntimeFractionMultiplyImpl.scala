package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.RuntimeFractionMultiplyOps

trait RuntimeFractionMultiplyImpl[G] extends RuntimeFractionMultiplyOps[G] {
  this: RuntimeFractionMultiply[G] =>

  override def t: Type[G] = TRuntimeFraction[G]()
  override def precedence: Int = Precedence.ATOMIC

  override def layout(implicit ctx: Ctx): Doc =
    left.show <> Text(".multiply(") <> right <> Text(")")
}