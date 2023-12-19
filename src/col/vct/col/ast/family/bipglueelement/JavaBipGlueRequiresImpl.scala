package vct.col.ast.family.bipglueelement

import vct.col.ast.JavaBipGlueRequires
import vct.col.print._

trait JavaBipGlueRequiresImpl[G] { this: JavaBipGlueRequires[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(port.show <> ".requires(" <> Doc.args(others) <> ");")
}
