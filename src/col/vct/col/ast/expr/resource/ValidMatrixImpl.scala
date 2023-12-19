package vct.col.ast.expr.resource

import vct.col.ast.{TBool, Type, ValidMatrix}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait ValidMatrixImpl[G] { this: ValidMatrix[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("\\matrix(") <> Doc.args(Seq(mat, w, h)) <> ")")
}