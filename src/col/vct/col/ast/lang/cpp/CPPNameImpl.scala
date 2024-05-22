package vct.col.ast.lang.cpp

import vct.col.ast.CPPName
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CPPNameOps

trait CPPNameImpl[G] extends CPPNameOps[G] { this: CPPName[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(name)
}