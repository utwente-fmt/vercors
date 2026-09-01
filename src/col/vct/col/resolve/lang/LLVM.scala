package vct.col.resolve.lang

import vct.col.ast._
import vct.col.origin.{NonConstantStructIndex, Origin}
import vct.col.ref.Ref
import vct.col.resolve.NoSuchNameError
import vct.col.resolve.ctx.ReferenceResolutionContext
import vct.col.resolve.ctx._
import vct.result.VerificationError.UserError

object LLVM {
  def findCallable[G](
      name: String,
      ctx: ReferenceResolutionContext[G],
  ): Option[LLVMCallable[G]] = {
    // look in context
    val callable = ctx.stack.flatten.map {
      case RefLLVMGlobalSpecification(decl, i) =>
        decl.data.get(i) match {
          case f: LLVMSpecFunction[G] if f.name == name => Some(f)
          case _ => None
        }
      case _ => None
    }.collectFirst { case Some(f) => f }
    // if not present in context, might find it in the call site of the current function definition
    callable match {
      case Some(callable) => Some(callable)
      case None =>
        ctx.currentResult.get match {
          case RefLLVMFunctionDefinition(decl) =>
            decl.contract match {
              case contract: VCLLVMFunctionContract[_] =>
                contract.invokableRefs.find(ref => ref._1 == name) match {
                  case Some(ref) => Some(ref._2.decl)
                  case None => None
                }
              case _ => None
            }
          case _ => None
        }
    }
  }

  def scanBlocks[G](node: Node[G]): Map[LabelDecl[G], LLVMBasicBlock[G]] = {
    node.collect { case b: LLVMBasicBlock[G] => (b.label, b) }.toMap
  }

  def getGEPResultType[G](gep: LLVMGetElementPointer[G]): Option[Type[G]] = {
    var currentType = gep.structureType
    gep.indices.tail.foreach { i =>
      currentType match {
        case struct: LLVMTStruct[G] =>
          val value =
            i match {
              case value: LLVMIntegerValue[G] => value.value.intValue
              case value: IntegerValue[G] => value.value.intValue
              case _ => throw NonConstantStructIndex(gep.o)
            }
          currentType = struct.ref.decl.elements(value).t
        case array: LLVMTArray[G] => currentType = array.elementType
        case vector: LLVMTVector[G] => currentType = vector.elementType
        // We don't know how to index other types
        case _ => return None
      }
    }
    Some(currentType)
  }

}
