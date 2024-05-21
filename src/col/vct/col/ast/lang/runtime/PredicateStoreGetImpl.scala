package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.PredicateStoreGetOps

trait PredicateStoreGetImpl[G] extends PredicateStoreGetOps[G] {
  this: PredicateStoreGet[G] =>

  override def t: Type[G] = TArray[G](TClass[G](cls, Nil))

  override def layout(implicit ctx: Ctx): Doc =
    Text("predicateStore.get(") <> threadId.show <> Text(")")

}