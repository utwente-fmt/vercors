package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.RuntimeConcurrentHashMapOps

trait RuntimeConcurrentHashMapImpl[G] extends RuntimeConcurrentHashMapOps[G] {
  this: RuntimeConcurrentHashMap[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("Map<") <> keyType.show <> "," <+> valueType.show <> ">"
}