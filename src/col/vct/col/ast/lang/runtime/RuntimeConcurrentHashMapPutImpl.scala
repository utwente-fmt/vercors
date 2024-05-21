package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.RuntimeConcurrentHashMapPutOps

trait RuntimeConcurrentHashMapPutImpl[G] extends RuntimeConcurrentHashMapPutOps[G] {
  this: RuntimeConcurrentHashMapPut[G] =>

  override val t: Type[G] = TVoid[G]()

  override def layout(implicit ctx: Ctx): Doc = hm.show <> ".put(" <> key.show <> "," <+> value.show <> ")"
}