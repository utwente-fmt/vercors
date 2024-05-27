package vct.col.ast.expr.misc

import vct.col.ast.{StringValue, TString, Type}
import vct.col.print._
import vct.col.ast.ops.StringValueOps

trait StringValueImpl[G] extends StringValueOps[G] {
  this: StringValue[G] =>
  override def t: Type[G] = TString()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("\"") <> value <> "\""
}
