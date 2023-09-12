package vct.col.ast.lang

import vct.col.ast.CPPTLambda
import vct.col.print.{Ctx, Doc, Text}

trait CPPTLambdaImpl[G] { this: CPPTLambda[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("lambda_expression")
}