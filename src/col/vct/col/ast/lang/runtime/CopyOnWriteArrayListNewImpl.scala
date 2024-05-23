package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.CopyOnWriteArrayListNewOps

trait CopyOnWriteArrayListNewImpl[G] extends CopyOnWriteArrayListNewOps[G] {
  this: CopyOnWriteArrayListNew[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("new") <+> t.show <> "()"

}