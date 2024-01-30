package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait RuntimeConcurrentHashMapGetOrDefaultImpl[G] {
  this: RuntimeConcurrentHashMapGetOrDefault[G] =>

  override val t: Type[G] = hm.t.asInstanceOf[RuntimeConcurrentHashMap[G]].valueType

  override def layout(implicit ctx: Ctx): Doc = hm.show <> ".getOrDefault(" <> key.show <> "," <+> default.show <> ")"
}