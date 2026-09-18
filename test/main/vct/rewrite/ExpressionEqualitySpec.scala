package vct.rewrite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import vct.col.ast._
import vct.col.ast.util.ExpressionEqualityCheck.AskSMTSolver
import vct.col.origin._
import vct.col.rewrite.InitialGeneration
import vct.col.util.AstBuildHelpers._

class ExpressionEqualitySpec extends AnyFlatSpec with Matchers {
  type G = InitialGeneration
  implicit val o: Origin = DiagnosticOrigin
  implicit val blame: Blame[VerificationFailure] = PanicBlame("")
  val xs_ = new Variable[G](TArray[G](TInt[G]()))
  val xs = Local[G](xs_.ref)

  val xmin_ =
    for (xmin <- 0 until 10)
      yield (new Variable[G](TInt[G]())(o.where(name=s"xmin$xmin")))
  val xmin: Seq[Expr[G]] =
    for (xmin <- xmin_)
      yield (Local[G](xmin.ref))

  val ns_ =
    for (n <- 0 until 10)
      yield (new Variable[G](TInt[G]())(o.where(name=s"ns$n")))
  val ns: Seq[Expr[G]] =
    for (n <- ns_)
      yield (Local[G](n.ref))
  val as_ =
    for (a <- 0 until 10)
      yield (new Variable[G](TInt[G]())(o.where(name=s"as$a")))
  val as: Seq[Expr[G]] =
    for (n <- as_)
      yield (Local[G](n.ref))


  def c(i: Int) = const[G](i)
  def abs(e: Expr[G]) = Select(e >= const(0), e, -e)

  it should "SMT should prove correct about 0<=x % 4 <4" in {
    val smt = AskSMTSolver(Seq(), as(0) % c(4) < c(4) && as(0) % c(4) >= c(0) )
    assert(smt.check())
  }

  it should "SMT should prove correct about (c def) -4 < x % 4 < 4 " in {
    val smt = AskSMTSolver(Seq(), TruncMod(as(0), c(4))(blame) < c(4) && TruncMod(as(0), c(4))(blame) > -c(4) )
    assert(smt.check())
  }

  it should "SMT should prove correct about (c def) 10%3==1 " in {
    val smt = AskSMTSolver(Seq(),
      TruncMod(c(10), c(3))(blame) === c(1) &&
        TruncMod(c(10), c(-3))(blame) === c(1) &&
        TruncMod(c(-10), c(3))(blame) === c(-1) &&
        TruncMod(c(-10), c(-3))(blame) === c(-1)
    )
    assert(smt.check())
  }

  it should "SMT should prove correct about (c def) 10/3==3 " in {
    val smt = AskSMTSolver(Seq(),
      TruncDiv(c(10), c(3))(blame) === c(3) &&
        TruncDiv(c(10), c(-3))(blame) === c(-3) &&
        TruncDiv(c(-10), c(3))(blame) === c(-3) &&
        TruncDiv(c(-10), c(-3))(blame) === c(3)
    )
    assert(smt.check())
  }
}