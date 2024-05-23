package vct.col.resolve.lang

import vct.col.ast._
import vct.col.resolve.NoSuchNameError
import vct.col.resolve.ctx.ReferenceResolutionContext
import vct.col.resolve.ctx._

object LLVM {

  def findCallable[G](
      name: String,
      ctx: ReferenceResolutionContext[G],
  ): Option[LlvmCallable[G]] = {
    // look in context
    val callable = ctx.stack.flatten.map {
      case RefLlvmGlobal(decl, i) =>
        decl.data.get(i) match {
          case f: LlvmSpecFunction[G] if f.name == name => Some(f)
          case _ => None
        }
      case _ => None
    }.collectFirst { case Some(f) => f }
    // if not present in context, might find it in the call site of the current function definition
    callable match {
      case Some(callable) => Some(callable)
      case None =>
        ctx.currentResult.get match {
          case RefLlvmFunctionDefinition(decl) =>
            decl.contract.invokableRefs.find(ref => ref._1 == name) match {
              case Some(ref) => Some(ref._2.decl)
              case None => None
            }
          case _ => None
        }
    }
  }

}
