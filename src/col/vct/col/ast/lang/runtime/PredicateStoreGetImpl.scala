package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait PredicateStoreGetImpl[G] {
  this: PredicateStoreGet[G] =>

  override def t: Type[G] = TArray[G](TClass[G](cls))

  override def layout(implicit ctx: Ctx): Doc =
    Text("predicateStore.get(") <> threadId.show <> Text(")")

}