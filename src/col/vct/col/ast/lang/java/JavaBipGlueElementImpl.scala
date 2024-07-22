package vct.col.ast.lang.java

import vct.col.ast.JavaBipGlueElement
import vct.col.ast.ops.JavaBipGlueElementFamilyOps

trait JavaBipGlueElementImpl[G] extends JavaBipGlueElementFamilyOps[G] {
  this: JavaBipGlueElement[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
