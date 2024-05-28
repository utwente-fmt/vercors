package vct.col.ast.lang.cpp

import vct.col.ast.{CPPLambdaRef, TRef, Type}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CPPLambdaRefOps

trait CPPLambdaRefImpl[G] extends CPPLambdaRefOps[G] {
  this: CPPLambdaRef[G] =>
  override lazy val t: Type[G] = TRef()

  override def layout(implicit ctx: Ctx): Doc = Text("VERCORS::LAMBDA")
}
