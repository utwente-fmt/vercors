package vct.col.ast.lang.cpp

import vct.col.ast.CPPLambdaDeclarator
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.CPPLambdaDeclaratorOps

trait CPPLambdaDeclaratorImpl[G] extends CPPLambdaDeclaratorOps[G] { this: CPPLambdaDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("(") <> Doc.args(params) <> ")")
}