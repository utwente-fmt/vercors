package vct.col.ast.expr.heap.read

import vct.col.ast.{AddrOf, TPointer, Type}
import vct.col.print._

trait AddrOfImpl[G] { this: AddrOf[G] =>
  override def t: Type[G] = TPointer(e.t)

  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("&") <> assoc(e)
}