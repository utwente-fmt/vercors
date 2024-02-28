package vct.col.ast.unsorted

import vct.col.ast.{LLVMLoad, Type}
import vct.col.ast.ops.LLVMLoadOps

trait LLVMLoadImpl[G] extends LLVMLoadOps[G] { this: LLVMLoad[G] =>
  override lazy val t: Type[G] = this.load_type
}
