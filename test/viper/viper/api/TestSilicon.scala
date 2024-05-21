package viper.api

import vct.col.util.AstBuildHelpers._
import vct.col.ast._

import java.nio.file.Paths
import vct.col.origin.DiagnosticOrigin
import viper.api.backend.silicon.Silicon
import viper.silver.{ast => silver}

class TestSilicon extends VerifySpec[(silver.Program, Map[Int, Node[_]])](Silicon(Map.empty, Resources.getZ3Path)) {
  vercors should "verify an empty program" in program {
    Program(Nil)(noErrors)
  }

  vercors should "not verify a method with postcondition false" in procedure(
    ensures = ff, blame = ExpectError()
  )

  val r = new Variable[G](TRef())
  var i = new Variable[G](TInt())

  vercors should "report insufficient permission on a dereference" in procedure(
    args=Seq(r), body=Scope(Seq(i), i <~ (r.get~>int)(ExpectError(), DiagnosticOrigin))
  )

  vercors should "verify a dereference with sufficient permission" in procedure(
    args=Seq(r), requires=Perm(SilverFieldLocation(r.get, int.ref), WritePerm()),
    body=Scope(Seq(i), i <~ r.get~>int)
  )

  vercors should "report assignment failed when there is insufficient permission to assign to a field" in procedure(
    args=Seq(r), body=(r.get~>int <~ const(0))(ExpectError(), DiagnosticOrigin)
  )

  vercors should "assign a field with sufficient permission" in procedure(
    args=Seq(r), requires=Perm(SilverFieldLocation(r.get, int.ref), WritePerm()),
    body=r.get~>int <~ const(0)
  )

  vercors should "verify true assertions" in procedure(
    body=Assert[G](tt)(noErrors)
  )

  vercors should "report the failure of false assertions" in procedure(
    body=Assert[G](ff)(ExpectError())
  )

  val rs = new Variable[G](TSeq(TRef()))

  vercors should "report the failure of assertion of potentially non-injective staralls" in procedure(
    args=Seq(rs),
    body=Assert[G](
      Starall(Seq(i), Seq(),
        Implies(i.get >= const(0) && i.get < SilverSeqSize(rs.get),
          Perm(SilverFieldLocation[G]((rs.get @@ i.get), int.ref), WritePerm())))(ExpectError()))(noErrors)
  )

  val p = new Variable[G](TRational())

  vercors should "report the failure of an assertion with a negative permission value" in procedure(
    args=Seq(r, p), body=Block[G](Seq(Assert[G](Perm(SilverFieldLocation(r.get, int.ref), p.get))(ExpectError())))
  )

  vercors should "report insufficient permission to exhale when asserting too much permission" in procedure(
    args=Seq(r), body=Block[G](Seq(Assert[G](Perm(SilverFieldLocation(r.get, int.ref), WritePerm()))(ExpectError())))
  )

  vercors should "verify a valid exhale of permission" in procedure(
    args=Seq(r), requires=Perm(SilverFieldLocation[G](r.get, int.ref), WritePerm()),
    body=Block[G](Seq(Exhale[G](Perm(SilverFieldLocation(r.get, int.ref), WritePerm()))(noErrors)))
  )

  vercors should "report insufficient permission to exhale when exhaling too much permission" in procedure(
    args=Seq(r), body=Exhale[G](Perm(SilverFieldLocation(r.get, int.ref), WritePerm()))(ExpectError())
  )

  val validPred = new Predicate(Seq(), Some(Perm[G](SilverFieldLocation(r.get, int.ref), WritePerm())))
  val invalidPred = new Predicate(Seq(), Some(Eq(r.get~>int, const(5))))
}
