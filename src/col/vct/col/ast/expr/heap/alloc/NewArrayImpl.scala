package vct.col.ast.expr.heap.alloc

import hre.util.FuncTools
import vct.col.ast.{NewArray, TArray, Type}
import vct.col.print._

trait NewArrayImpl[G] { this: NewArray[G] =>
  override lazy val t: Type[G] = FuncTools.repeat[Type[G]](TArray(_), dims.size + moreDims, element)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> element <> Doc.fold(dims.map(Text("[") <> _ <> "]"))(_ <> _) <> "[]".repeat(moreDims)
}