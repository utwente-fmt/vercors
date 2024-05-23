package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.RuntimeConcurrentHashMapContainsKeyOps

trait RuntimeConcurrentHashMapContainsKeyImpl[G] extends RuntimeConcurrentHashMapContainsKeyOps[G] {
  this: RuntimeConcurrentHashMapContainsKey[G] =>

  override val t: Type[G] = TBool[G]()

  override def layout(implicit ctx: Ctx): Doc = hm.show <> ".containsKey(" <> key.show <> ")"
}