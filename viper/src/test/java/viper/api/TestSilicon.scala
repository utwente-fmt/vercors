package viper.api

import vct.col.ast.AstBuildHelpers._
import vct.col.ast._

import java.nio.file.Paths
import vct.col.ast.Constant._

class TestSilicon extends VerifySpec(Silicon(Map.empty, Paths.get("/home/pieter/vercors/src/main/universal/res/deps/z3/4.8.6/Linux/x86_64/bin/z3"))) {
  vercors should "verify an empty program" in program {
    Program(Seq())(noErrors)
  }

  vercors should "not verify a method with postcondition false" in procedure(
    ensures = false, blame = ExpectPostconditionFailed()
  )

  val r = new Variable(TRef())
  var i = new Variable(TInt())

  vercors should "report insufficient permission on a dereference" in procedure(
    args=Seq(r), body=Scope(Seq(i), i <~ (r.get->int)(ExpectSilverInsufficientPermission(), DiagnosticOrigin))
  )

  vercors should "verify a dereference with sufficient permission" in procedure(
    args=Seq(r), requires=SilverPerm(r.get, int.ref, WritePerm()),
    body=Scope(Seq(i), i <~ r.get->int)
  )

  vercors should "report assignment failed when there is insufficient permission to assign to a field" in procedure(
    args=Seq(r), body=(r.get->int <~ 0)(ExpectSilverAssignFailed(), DiagnosticOrigin)
  )

  vercors should "assign a field with sufficient permission" in procedure(
    args=Seq(r), requires=SilverPerm(r.get, int.ref, WritePerm()),
    body=r.get->int <~ 0
  )

  vercors should "verify true assertions" in procedure(
    body=Assert(true)(noErrors)
  )

  vercors should "report the failure of false assertions" in procedure(
    body=Assert(false)(ExpectAssertFailed())
  )

  val rs = new Variable(TSeq(TRef()))

  vercors should "report the failure of assertion of potentially non-injective staralls" in procedure(
    args=Seq(rs),
    body=Assert(
      Starall(Seq(i), Seq(),
        Implies(i.get >= 0 && i.get < Size(rs.get),
          SilverPerm(rs.get @@ i.get, int.ref, WritePerm()))))
      (ExpectAssertFailed())
  )

  val p = new Variable(TRational())

  vercors should "report the failure of an assertion with a negative permission value" in procedure(
    args=Seq(r, p), body=Block(Seq(Assert(SilverPerm(r.get, int.ref, p.get))(ExpectAssertFailed())))
  )

  vercors should "report insufficient permission to exhale when asserting too much permission" in procedure(
    args=Seq(r), body=Block(Seq(Assert(SilverPerm(r.get, int.ref, WritePerm()))(ExpectAssertFailed())))
  )

  vercors should "verify a valid exhale of permission" in procedure(
    args=Seq(r), requires=SilverPerm(r.get, int.ref, WritePerm()),
    body=Block(Seq(Exhale(SilverPerm(r.get, int.ref, WritePerm()))(noErrors)))
  )

  vercors should "report insufficient permission to exhale when exhaling too much permission" in procedure(
    args=Seq(r), body=Exhale(SilverPerm(r.get, int.ref, WritePerm()))(ExpectExhaleFailed())
  )

  val validPred = new Predicate(Seq(), Some(SilverPerm(r.get, int.ref, WritePerm())))
  val invalidPred = new Predicate(Seq(), Some(Eq(r.get->int, 5)))
}
