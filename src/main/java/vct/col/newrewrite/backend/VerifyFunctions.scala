package vct.col.newrewrite.backend

import vct.col.ast._
import vct.col.ast.RewriteHelpers._
import vct.col.origin.{FileSpanningOrigin, PanicBlame}
import vct.col.rewrite.{Generation, Rewriter, Rewritten}

case class VerifyFunctions[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(context: Verification[Pre]): Verification[Post] =
    context.rewrite(tasks = context.tasks.flatMap { task =>
      Seq(
        task.rewrite(program = Program(
          declarations = task.program.declarations.collect { case f: Function[Pre] => f }.map(verifyFunction),
          rootClass = None
        )(PanicBlame("no more coercions"))(FileSpanningOrigin)),
        task.rewrite(),
      )
    })

  def verifyFunction(f: Function[Pre]): Procedure[Post] = {

  }
}
