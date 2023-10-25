package vct.col.util

import vct.col.ast.{Node, Program}
import vct.col.origin.Origin
import vct.result.VerificationError

object CurrentProgramContext {
  def bareNodeContext(err: VerificationError, node: Node[_], message: String): String =
    err.context[CurrentProgramContext] match {
      case Some(ctx) => ctx.program.bareMessageInContext(node, message)
      case None => node.toString + "\n" + Origin.HR + message
    }

  def nodeContext(err: VerificationError, node: Node[_], message: String): String =
    Origin.BOLD_HR + bareNodeContext(err, node, message) + "\n" + Origin.BOLD_HR
}

trait CurrentProgramContext extends VerificationError.Context {
  def program: Program[_]
}
