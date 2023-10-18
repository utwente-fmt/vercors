package vct.col.ast.lang

import vct.col.ast.{
  JavaClass,
  JavaClassOrInterface,
  JavaStringValue,
  JavaTClass,
  Type,
}
import vct.col.print.{Ctx, Doc, Text}
import vct.result.VerificationError.Unreachable

trait JavaStringValueImpl[G] {
  this: JavaStringValue[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("\"") <> data <> Text("\"")
}
