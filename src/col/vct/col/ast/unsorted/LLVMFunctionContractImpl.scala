package vct.col.ast.unsorted

import vct.col.ast.{LLVMFunctionContract, Variable}
import vct.col.ast.ops.LLVMFunctionContractFamilyOps
import vct.col.print._

trait LLVMFunctionContractImpl[G] extends LLVMFunctionContractFamilyOps[G] {
  this: LLVMFunctionContract[G] =>
  def givenArgs: Seq[Variable[G]]
  def yieldsArgs: Seq[Variable[G]]
}
