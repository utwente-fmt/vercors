package vct.helper

import vct.col.ast.{Program, Rewriter}

object RewriteTestHelper {
  def test(rewriter: Rewriter, input: Program, expectedOutput: Program): Unit = {
    val programActualOutput = rewriter.rewrite(input)
    AstComparer.astProgramEquals(programActualOutput, expectedOutput)
  }
}
