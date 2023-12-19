package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibIsInt, TBool, Type}
import vct.col.print._

trait SmtlibIsIntImpl[G] { this: SmtlibIsInt[G] =>
  override def t: Type[G] = TBool()
  override def layout(implicit ctx: Ctx): Doc = Text("is_int(") <> arg <> ")"
}
