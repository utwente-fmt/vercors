package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.RuntimeFractionSubstractOps

trait RuntimeFractionSubstractImpl[G] extends RuntimeFractionSubstractOps[G] {
  this: RuntimeFractionSubstract[G] =>

  override def t: Type[G] = TRuntimeFraction[G]()
  override def precedence: Int = Precedence.ATOMIC

  override def layout(implicit ctx: Ctx): Doc =
    left.show <> Text(".subtract(") <> right <> Text(")")
}