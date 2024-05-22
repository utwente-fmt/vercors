package vct.col.util

import vct.col.ast.{Node, Program}
import vct.col.origin.Origin
import vct.result.{HasContext, VerificationError}

trait CurrentProgramContext extends VerificationError.Context with HasContext {
  def program: Program[_]
  override def contextText: String = program.highlight(program).contextText
  def highlight(node: Node[_]): HasContext = program.highlight(node)
}
