package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.ObjectIsArrayOps

trait ObjectIsArrayImpl[G] extends ObjectIsArrayOps[G] {
  this: ObjectIsArray[G] =>

  override val t: Type[G] = TBool[G]()

  override def layout(implicit ctx: Ctx): Doc = input.show <> ".getClass().isArray()"
}