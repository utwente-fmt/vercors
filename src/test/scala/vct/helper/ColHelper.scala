package vct.helper

import org.scalatest.Assertions.fail
import vct.col.ast.util.Compare
import vct.col.ast.{Node, Program, Rewriter}

object ColHelper {
  def assertEquals(left: Node, right: Node): Unit = {
    Compare.getIsomorphism(left, right) match {
      case Left(diffs) =>
        diffs.foreach {
          case (left, right) => fail(s"ASTs are different:\n$left\n\n$right")
        }
        fail("Failing")
      case Right(_) => // ok
    }
  }
}
