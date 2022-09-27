package vct.col.ast.util

import org.scalatest.flatspec.AnyFlatSpec
import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.util.AstBuildHelpers._
import vct.col.util.Compare

class TestCompare extends AnyFlatSpec {
  sealed trait G
  implicit val o: Origin = DiagnosticOrigin

  it should "judge equal non-referential expressions as equal without mappings" in {
    assert(Compare.getIsomorphism(
      left = const(1) + (const(2) - const(3)),
      right = const(1) + (const(2) - const(3)),
    ) == Right(Map.empty))
  }

  it should "judge equal declarations without references as equal with mappings" in {
    val left = new Procedure[G](TVoid(), Nil, Nil, Nil, None, ApplicableContract(UnitAccountedPredicate(tt), UnitAccountedPredicate(tt), tt, Nil, Nil, Nil, None)(null))(null)
    val right = new Procedure[G](TVoid(), Nil, Nil, Nil, None, ApplicableContract(UnitAccountedPredicate(tt), UnitAccountedPredicate(tt), tt, Nil, Nil, Nil, None)(null))(null)
    assert(Compare.getIsomorphism(left, right) == Right(Map(left -> right)))
  }

  it should "judge equal expressions with references as equal with the proper mapping" in {
    val left = new Variable[G](TInt())
    val leftVal = Local[G](left.ref)
    val right = new Variable[G](TInt())
    val rightVal = Local[G](right.ref)
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
    val (a, b, c) = (new Variable[G](TInt()), new Variable[G](TInt()), new Variable[G](TInt()))
    val (valA, valB, valC) = (Local[G](a.ref), Local[G](b.ref), Local[G](c.ref))

    assert(Compare.getIsomorphism(
      left = valA + valB,
      right = valC + valC,
    ) == Left(Seq((b, c))))
  }
}
