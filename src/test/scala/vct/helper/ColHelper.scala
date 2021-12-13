package vct.helper

import org.scalatest.Assertions.fail
import vct.col.ast.{Node, Program}
import vct.col.util.Compare

object ColHelper {
  def assertEquals[L, R](left: Node[L], right: Node[R]): Unit = {
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
