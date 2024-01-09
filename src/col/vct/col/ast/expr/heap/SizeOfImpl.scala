package vct.col.ast.expr.heap

import vct.col.ast.{SizeOf, TCInt, Type}
import vct.col.print._
import vct.col.ast.ops.SizeOfOps

trait SizeOfImpl[G] extends SizeOfOps[G] { this: SizeOf[G] =>
  override def t: Type[G] = TCInt()

  override def layout(implicit ctx: Ctx): Doc =
    Text("sizeof(") <> tname <> ")"
}