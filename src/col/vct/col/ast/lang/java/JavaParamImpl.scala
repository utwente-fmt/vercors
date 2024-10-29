package vct.col.ast.lang.java

import vct.col.ast.JavaParam
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.{JavaParamOps, JavaParamFamilyOps}

trait JavaParamImpl[G] extends JavaParamOps[G] with JavaParamFamilyOps[G] {
  this: JavaParam[G] =>
  override def layout(implicit ctx: Ctx): Doc = t.show <+> name

}
