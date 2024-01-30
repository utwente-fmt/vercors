package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait RuntimeConcurrentHashMapImpl[G] {
  this: RuntimeConcurrentHashMap[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("ConcurrentHashMap<") <> keyType.show <> "," <+> valueType.show <> ">"
}