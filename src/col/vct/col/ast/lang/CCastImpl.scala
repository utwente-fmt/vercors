package vct.col.ast.lang

import vct.col.ast.{CCast, TNotAValue, Type}
import vct.col.print.Doc.{arg, fold}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait CCastImpl[G] { this: CCast[G] =>
  override def t: Type[G] = castType

  override def precedence = Precedence.ATOMIC

  override def layout(implicit ctx: Ctx): Doc = Text("(") <> t <> ")" <> assoc(expr)
}