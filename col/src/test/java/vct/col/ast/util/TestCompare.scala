package vct.col.ast.util

import org.scalatest.flatspec.AnyFlatSpec
import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.util.AstBuildHelpers._
import vct.col.util.Compare

class TestCompare extends AnyFlatSpec {
  implicit val o: Origin = DiagnosticOrigin

  it should "judge equal non-referential expressions as equal without mappings" in {
    assert(Compare.getIsomorphism(
      left = const(1) + (const(2) - const(3)),
      right = const(1) + (const(2) - const(3)),
    ) == Right(Map.empty))
  }

  it should "judge equal declarations without references as equal without mappings" in {
    assert(Compare.getIsomorphism(
      new Procedure(TVoid(), Nil, Nil, Nil, None, ApplicableContract(tt, tt, tt, Nil, Nil, Nil))(null),
      new Procedure(TVoid(), Nil, Nil, Nil, None, ApplicableContract(tt, tt, tt, Nil, Nil, Nil))(null),
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
      left = const(1) + const(2) * const(3),
      right = const(3) + const(2) * const(1),
    ).isLeft)
  }

  it should "indicate the inequal node in a nested expression" in {
    assert(Compare.getIsomorphism(
      left = const(1) * const(2) + const(3) * const(100),
      right = const(1) * const(2) + const(3) * const(200),
    ) == Left(Seq((const(100), const(200)))))
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
