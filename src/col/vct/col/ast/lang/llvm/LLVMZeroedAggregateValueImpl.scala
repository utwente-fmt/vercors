package vct.col.ast.lang.llvm

import vct.col.ast.{Type, LLVMZeroedAggregateValue}
import vct.col.ast.ops.LLVMZeroedAggregateValueOps
import vct.col.print._

trait LLVMZeroedAggregateValueImpl[G] extends LLVMZeroedAggregateValueOps[G] {
  this: LLVMZeroedAggregateValue[G] =>
  override def value: Unit = ()
  override def t: Type[G] = aggregateType
  // override def layout(implicit ctx: Ctx): Doc = ???
}
