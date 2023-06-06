package vct.col.ast.family.bipglueelement

import vct.col.ast.JavaBipGlueDataWire
import vct.col.print._

trait JavaBipGlueDataWireImpl[G] { this: JavaBipGlueDataWire[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(dataOut.show <> ".to(" <> Doc.arg(dataIn) <> ");")
}
