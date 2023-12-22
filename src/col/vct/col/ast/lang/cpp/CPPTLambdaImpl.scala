package vct.col.ast.lang.cpp

import vct.col.ast.CPPTLambda
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CPPTLambdaOps

trait CPPTLambdaImpl[G] extends CPPTLambdaOps[G] { this: CPPTLambda[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("VERCORS::LAMBDA")
}