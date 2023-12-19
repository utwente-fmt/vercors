package vct.col.ast.family.bipglueelement

import vct.col.ast.JavaBipGlueSynchron
import vct.col.print._

trait JavaBipGlueSynchronImpl[G] { this: JavaBipGlueSynchron[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("synchron(") <> Doc.arg(port0) <> ").to(" <> Doc.arg(port1) <> ");")
}