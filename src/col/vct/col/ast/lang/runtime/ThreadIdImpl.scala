package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.ThreadIdOps

trait ThreadIdImpl[G] extends ThreadIdOps[G] {
  this: ThreadId[G] =>


  override def t: Type[G] = TLong[G]()

  def objectLayout(implicit ctx: Ctx) : Doc = {
    obj match {
      case Some(value) => value.show <> Text(".getId()")
      case None => Text("Thread.currentThread().getId()")
    }
  }


  override def layout(implicit ctx: Ctx): Doc = objectLayout
}