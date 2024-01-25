package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}

trait PredicateStoreImpl[G] {
  this: PredicateStore[G] =>




  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("private static ConcurrentHashMap<Long, CopyOnWriteArrayList<") <> storeType <> Text(">> predicateStore = new ConcurrentHashMap();"))

}