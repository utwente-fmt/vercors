package vct.col.ast.lang.cpp

import vct.col.ast.{CPPLiteralArray, TNotAValue, Type}
import vct.col.print.Doc.{arg, fold}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.CPPLiteralArrayOps

trait CPPLiteralArrayImpl[G] extends CPPLiteralArrayOps[G] {
  this: CPPLiteralArray[G] =>
  override def t: Type[G] = new TNotAValue()

  override def precedence = Precedence.ATOMIC

  override def layout(implicit ctx: Ctx): Doc =
    Text("{") <> arg(fold(exprs)(_ <> "," <+> _)) <> "}"
}
