package vct.col.ast.lang

import vct.col.ast.CPPLambdaDeclarator
import vct.col.print.{Ctx, Doc, Group, Text}

trait CPPLambdaDeclaratorImpl[G] { this: CPPLambdaDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("(") <> Doc.args(params) <> ")")
}