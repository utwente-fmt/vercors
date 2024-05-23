package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.RuntimeNewConcurrentHashMapOps

trait RuntimeNewConcurrentHashMapImpl[G] extends RuntimeNewConcurrentHashMapOps[G] {
  this: RuntimeNewConcurrentHashMap[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("Collections.synchronizedMap(new WeakHashMap<>())")
}