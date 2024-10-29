package vct.col.ast.family.bipglueelement

import vct.col.ast.JavaBipGlueAccepts
import vct.col.print._
import vct.col.ast.ops.JavaBipGlueAcceptsOps

trait JavaBipGlueAcceptsImpl[G] extends JavaBipGlueAcceptsOps[G] {
  this: JavaBipGlueAccepts[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(port.show <> ".accepts(" <> Doc.args(others) <> ");")
}
