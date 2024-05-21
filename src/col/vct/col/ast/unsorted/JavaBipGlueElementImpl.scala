package vct.col.ast.unsorted

import vct.col.ast.JavaBipGlueElement
import vct.col.ast.ops.JavaBipGlueElementFamilyOps
import vct.col.print._

trait JavaBipGlueElementImpl[G] extends JavaBipGlueElementFamilyOps[G] { this: JavaBipGlueElement[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
