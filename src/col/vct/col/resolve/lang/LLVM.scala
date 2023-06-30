package vct.col.resolve.lang

import vct.col.ast._
import vct.col.resolve.NoSuchNameError
import vct.col.resolve.ctx.ReferenceResolutionContext
import vct.col.resolve.ctx._
object LLVM {

  def findCallable[G](name: String, ctx: ReferenceResolutionContext[G]): Option[LlvmCallable[G]] = {
    val callable = ctx.stack.flatten.collectFirst {
      case RefLlvmGlobal(decl) if decl.data.nonEmpty => decl.data.get.collectFirst {
        case f: LlvmSpecFunction[G] if f.name == name => f
      }
    }
    callable.get match {
      case Some(callable) => Some(callable)
      case None => ctx.currentResult.get match {
        case RefLlvmFunctionDefinition(decl) =>
          decl.contract.invokableRefs.find(ref => ref._1 == name) match {
            case Some(ref) => Some(ref._2.decl)
            case None => None
          }
      }
    }
  }


}
