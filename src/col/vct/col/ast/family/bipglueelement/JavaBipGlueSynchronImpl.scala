package vct.col.ast.family.bipglueelement

import vct.col.ast.JavaBipGlueSynchron
import vct.col.print._
import vct.col.ast.ops.JavaBipGlueSynchronOps

trait JavaBipGlueSynchronImpl[G] extends JavaBipGlueSynchronOps[G] { this: JavaBipGlueSynchron[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("synchron(") <> Doc.arg(port0) <> ").to(" <> Doc.arg(port1) <> ");")
}