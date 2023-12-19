package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibToInt, TInt, Type}
import vct.col.print._

trait SmtlibToIntImpl[G] { this: SmtlibToInt[G] =>
  override def t: Type[G] = TInt()
  override def layout(implicit ctx: Ctx): Doc = Text("to_int(") <> arg <> ")"
}
