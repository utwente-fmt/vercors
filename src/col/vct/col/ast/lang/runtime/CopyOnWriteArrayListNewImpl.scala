package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait CopyOnWriteArrayListNewImpl[G] {
  this: CopyOnWriteArrayListNew[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("new") <+> t.show <> "()"

}