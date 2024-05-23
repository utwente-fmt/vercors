package vct.col.ast.lang.cpp

import vct.col.ast.CPPSigned
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CPPSignedOps

trait CPPSignedImpl[G] extends CPPSignedOps[G] { this: CPPSigned[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("signed")
}