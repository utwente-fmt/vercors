package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait RuntimeConcurrentHashMapPutImpl[G] {
  this: RuntimeConcurrentHashMapPut[G] =>

  override val t: Type[G] = TVoid[G]()

  override def layout(implicit ctx: Ctx): Doc = hm.show <> ".put(" <> key.show <> "," <+> value.show <> ")"
}