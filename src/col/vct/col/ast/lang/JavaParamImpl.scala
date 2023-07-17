package vct.col.ast.lang

import vct.col.ast.JavaParam
import vct.col.print.{Ctx, Doc, Text}

trait JavaParamImpl[G] { this: JavaParam[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(t + " " + name)

}
