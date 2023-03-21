package vct.test.unit

import org.scalatest.flatspec.AnyFlatSpec
import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.util.AstBuildHelpers.const

class NodeMap extends AnyFlatSpec {
  implicit val o: Origin = DiagnosticOrigin
  sealed trait G

  it should "map all the values in a tree" in {
    assert(
      Plus[G](const(1), Minus(const(2), const(2))).map {
        case IntegerValue(n) => n
      }.toIndexedSeq == Seq(1, 2, 2)
    )
  }

  it should "not recurse into nodes that are part of the match" in {
    assert(
      Plus[G](Minus(const(1), const(2)), Plus(const(3), const(4))).map {
        case IntegerValue(n) => n
        case Minus(_, _) => -1
      }.toIndexedSeq == Seq(-1, 3, 4)
    )
  }

  it should "flatMap all the values in a tree" in {
    assert(
      Plus[G](const(1), Plus(const(2), const(3))).flatMap {
        case IntegerValue(n) => Seq(n, -1)
      }.toIndexedSeq == Seq(1, -1, 2, -1, 3, -1)
    )
  }

  it should "not recurse into nodes that are part of the match for flatMap" in {
    Plus[G](Plus(const(1), Minus(const(2), const(3))), Plus(const(4), const(5))).flatMap {
      case IntegerValue(n) => Seq(n)
      case Minus(_, _) => Nil
    }.toIndexedSeq == Seq(1, 4, 5)
  }
}
