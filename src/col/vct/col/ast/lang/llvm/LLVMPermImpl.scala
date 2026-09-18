package vct.col.ast.lang.llvm

import vct.col.ast.{LLVMPerm, TBool, TResource, Type}
import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.ops.LLVMPermOps
import vct.col.print.Precedence

trait LLVMPermImpl[G] extends NodeFamilyImpl[G] with LLVMPermOps[G] {
  this: LLVMPerm[G] =>

  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.POSTFIX

}
