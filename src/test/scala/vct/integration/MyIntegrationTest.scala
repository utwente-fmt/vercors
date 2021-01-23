package vct.integration

import org.scalatest._
import vct.col.ast.`type`.ClassType
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.ActionBlock

// Would like to have this as an actual sbt integration test.
// But jacoco seems to ignore integration tests when using "testOnly", so we put it here for now.

class MyIntegrationTest extends FlatSpec with Matchers {
  "VerCors" should "verify fibonacci" in {
    vct.main.Main.main(Array("--silicon", "examples/manual/fibonacci.pvl"))
  }

  "An action block" should "successfully instantiate when given valid input" in {
    val hist = new ClassType("Some history")
    val frac = new ClassType("Some fraction")
    val proc = new ClassType("Some process")
    val act = new ClassType("Some action")
    val block = new ClassType("Some block")
    val map = Map[String, ASTNode](
      "a" -> new ClassType("a"),
      "b" -> new ClassType("b")
    )

    val ab = new ActionBlock(hist, frac, proc, act, map, block)
    ab.map.isEmpty should be(false)
    ab.map.count(_ => true) should be(2)
  }
}
