package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.GetClassCallOps

trait GetClassCallImpl[G] extends GetClassCallOps[G] {
  this: GetClassCall[G] =>
  override def precedence: Int = Precedence.ATOMIC

  override val t: Type[G] = TAnyClass[G]()

  override def layout(implicit ctx: Ctx): Doc = {
    if(obj.nonEmpty) {
      obj.get.show <> ".getClass()"
    } else{
      Text("getClass()")
    }
  }
}