package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.CopyOnWriteArrayListOps

trait CopyOnWriteArrayListImpl[G] extends CopyOnWriteArrayListOps[G] {
  this: CopyOnWriteArrayList[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("ArrayList<") <> listType.show <> ">"

}