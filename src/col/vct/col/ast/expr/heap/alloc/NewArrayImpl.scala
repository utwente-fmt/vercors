package vct.col.ast.expr.heap.alloc

import hre.util.FuncTools
import vct.col.ast.{NewArray, TArray, Type}
import vct.col.print._
import vct.col.ast.ops.NewArrayOps

trait NewArrayImpl[G] extends NewArrayOps[G] {
  this: NewArray[G] =>
  override lazy val t: Type[G] = FuncTools
    .repeat[Type[G]](TArray(_), dims.size + moreDims, element)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> element <> dims.map(dim => s"[$dim]").mkString <>
      "[]".repeat(moreDims)
}
