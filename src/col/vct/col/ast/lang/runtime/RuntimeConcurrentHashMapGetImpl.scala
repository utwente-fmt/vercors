package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.RuntimeConcurrentHashMapGetOps

trait RuntimeConcurrentHashMapGetImpl[G] extends RuntimeConcurrentHashMapGetOps[G] {
  this: RuntimeConcurrentHashMapGet[G] =>

  override lazy val t: Type[G] = hm.t.asInstanceOf[RuntimeConcurrentHashMap[G]].valueType

  override def layout(implicit ctx: Ctx): Doc = hm.show <> ".get(" <> key.show <> ")"
}