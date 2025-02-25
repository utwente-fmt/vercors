package vct.col.util

import vct.col.ast.Declaration
import vct.col.print.Doc
import vct.result.{Message, VerificationError}

case class ConstructingSuccessorOfContext(decl: Declaration[_])
    extends VerificationError.Context {
  override def tryMessageContext(
      message: String,
      err: VerificationError,
  ): Option[String] =
    err.context[CurrentRewriteProgramContext].map { ctx =>
      ctx.program.highlight(decl).messageInContext(message)
    }
}
