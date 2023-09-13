package vct.col.ast.lang

import vct.col.ast.{CPPLambdaRef, TRef, Type}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.resolve.ctx._

trait CPPLambdaRefImpl[G] { this: CPPLambdaRef[G] =>
  override lazy val t: Type[G] = TRef()

  override def layout(implicit ctx: Ctx): Doc = Text("lambda_ref")
}