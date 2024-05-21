package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.RuntimeConcurrentHashMapKeySetOps

trait RuntimeConcurrentHashMapKeySetImpl[G] extends RuntimeConcurrentHashMapKeySetOps[G] {
  this: RuntimeConcurrentHashMapKeySet[G] =>

  override val t: Type[G] = TBool[G]()

  override def layout(implicit ctx: Ctx): Doc = hm.show <> ".keySet()"
}