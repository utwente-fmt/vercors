package vct.col.ast.expr.resource

import vct.col.ast.{TBool, Type, ValidArray}
import vct.col.print._
import vct.col.ast.ops.ValidArrayOps

trait ValidArrayImpl[G] extends ValidArrayOps[G] {
  this: ValidArray[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("\\array(") <> Doc.args(Seq(arr, len)) <> ")")
}
