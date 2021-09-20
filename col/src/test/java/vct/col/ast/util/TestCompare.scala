package vct.col.ast.util

import org.scalatest.flatspec.AnyFlatSpec
import vct.col.ast._
import Constant._
import AstBuildHelpers._

class TestCompare extends AnyFlatSpec {
  implicit val o: Origin = DiagnosticOrigin

  it should "judge equal non-referential expressions as equal without mappings" in {
    assert(Compare.getIsomorphism(
      left = integer(1) + (integer(2) - integer(3)),
      right = integer(1) + (integer(2) - integer(3)),
    ) == Right(Map.empty))
  }

  it should "judge equal declarations without references as equal without mappings" in {
    assert(Compare.getIsomorphism(
      new Procedure(TVoid(), Nil, Nil, None, ApplicableContract(true, true, true, Nil, Nil, Nil))(null),
      new Procedure(TVoid(), Nil, Nil, None, ApplicableContract(true, true, true, Nil, Nil, Nil))(null),
    ) == Right(Map.empty[Declaration, Declaration]))
  }

  it should "judge equal expressions with references as equal with the proper mapping" in {
    val left = new Variable(TInt())
    val leftVal = Local(left.ref)
    val right = new Variable(TInt())
    val rightVal = Local(right.ref)
    assert(Compare.getIsomorphism(
      leftVal + leftVal * leftVal,
      rightVal + rightVal * rightVal,
    ) == Right(Map(left -> right)))
  }

  it should "not judge inequal expressions as equal" in {
    assert(Compare.getIsomorphism(
      left = integer(1) + integer(2) * integer(3),
      right = integer(3) + integer(2) * integer(1),
    ).isLeft)
  }

  it should "indicate the inequal node in a nested expression" in {
    assert(Compare.getIsomorphism(
      left = integer(1) * integer(2) + integer(3) * integer(100),
      right = integer(1) * integer(2) + integer(3) * integer(200),
    ) == Left(Seq((integer(100), integer(200)))))
  }

  it should "indicate the inequal node in a tree, but not the first available declaration reference" in {
    val (a, b, c) = (new Variable(TInt()), new Variable(TInt()), new Variable(TInt()))
    val (valA, valB, valC) = (Local(a.ref), Local(b.ref), Local(c.ref))

    assert(Compare.getIsomorphism(
      left = valA + valB,
      right = valC + valC,
    ) == Left(Seq((b, c))))
  }
}
