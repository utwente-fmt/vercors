package vct.col.ast.family.bipglueelement

import vct.col.ast.JavaBipGlueRequires
import vct.col.print._
import vct.col.ast.ops.JavaBipGlueRequiresOps

trait JavaBipGlueRequiresImpl[G] extends JavaBipGlueRequiresOps[G] {
  this: JavaBipGlueRequires[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(port.show <> ".requires(" <> Doc.args(others) <> ");")
}
