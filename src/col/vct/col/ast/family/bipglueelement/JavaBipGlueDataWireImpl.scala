package vct.col.ast.family.bipglueelement

import vct.col.ast.JavaBipGlueDataWire
import vct.col.print._
import vct.col.ast.ops.JavaBipGlueDataWireOps

trait JavaBipGlueDataWireImpl[G] extends JavaBipGlueDataWireOps[G] { this: JavaBipGlueDataWire[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(dataOut.show <> ".to(" <> Doc.arg(dataIn) <> ");")
}
