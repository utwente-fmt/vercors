package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.RuntimeFractionDiffOps

trait RuntimeFractionDiffImpl[G] extends RuntimeFractionDiffOps[G] {
  this: RuntimeFractionDiff[G] =>

  override def t: Type[G] = TRuntimeFraction[G]()
  override def precedence: Int = Precedence.ATOMIC

  override def layout(implicit ctx: Ctx): Doc =
    Text("new Fraction(") <> left <> Text(",") <> right <> Text(")")
}