package vct.col.ast.lang.llvm

import vct.col.ast.LLVMSretArg
import vct.col.ast.ops.LLVMSretArgOps
import vct.col.print.{Ctx, Doc, Text}

trait LLVMSretArgImpl[G] extends LLVMSretArgOps[G] {
  this: LLVMSretArg[G] =>

  override def layout(implicit ctx: Ctx): Doc = {
    Text("sret(") <> t.show <> Text(")")
  }
}
