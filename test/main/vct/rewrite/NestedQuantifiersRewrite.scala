package vct.rewrite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import vct.col.ast._
import vct.col.rewrite.SimplifyNestedQuantifiers
import vct.col.origin._
import vct.col.rewrite.InitialGeneration
import vct.col.util.AstBuildHelpers._
import vct.helper.ColHelper


class NestedQuantifiersRewrite extends AnyFlatSpec with Matchers {
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

  def block(e: Expr[G], c: Option[Expr[G]]=None) =
    Scope[G]((xs_ +: ns_)++ xmin_ ++ as_,
      Inhale[G](if(c.isEmpty) e else And(c.get, e))
    )

  def notRewrite(bounds: Seq[(Expr[G], Expr[G])],
                 index: Seq[Local[G]] => Expr[G],
                 extra_cond: Option[Seq[Local[G]] => Expr[G]] = None,
                 c: Option[Expr[G]]=None
                ): Unit = {
    testRewriteForall(bounds, index, None, extra_cond, c)
  }

  def correctNumbers(bounds: Seq[(Expr[G], Expr[G])],
                     index: Seq[Local[G]] => Expr[G],
                     res_domain: Local[G] => Expr[G],
                     extra_cond: Option[Seq[Local[G]] => Expr[G]] = None,
                     c: Option[Expr[G]]=None
                    ): Unit = {
    testRewriteForall(bounds, index, Some(res_domain), extra_cond, c)
  }

  // If res_domain is None they should not be equal
  def testRewriteForall(bounds: Seq[(Expr[G], Expr[G])],
                     index: Seq[Local[G]] => Expr[G],
                     res_domain: Option[Local[G] => Expr[G]],
                     extra_cond: Option[Seq[Local[G]] => Expr[G]] = None,
                     c: Option[Expr[G]]=None
                    ): Unit = {
    val vars = Seq.fill(bounds.size)(TInt[G])

    val before = block(foralls[G](vars,
      {
        case locals =>
          var domain = locals.lazyZip(bounds).map(
            {case (x, (min, max)) => min <= x && x < max }
          ) . reduceLeft[Expr[G]]({case (l, r) => l && r})
          domain = extra_cond.map(c => domain && c(locals)).getOrElse(domain)
          domain ==> ArraySubscript(xs, index(locals))(blame) > const(0)
      },
    ), c)
    val rw = SimplifyNestedQuantifiers[G]()
    if(res_domain.isEmpty){
      ColHelper.assertEquals(rw.labelDecls.scope { rw.dispatch(before) }, before)
      return
    }

    val after = block(foralls[G](
      Seq(TInt()),
      {
        case Seq(xyz) =>
          res_domain.get (xyz) ==> ArraySubscript(xs, xyz)(blame) > const(0)
      },
      {
        case Seq(xyz) => Seq(Seq(ArraySubscript(xs, xyz)(blame)))
      }
    ), c)

    ColHelper.assertEquals(rw.labelDecls.scope { rw.dispatch(before) }, after)
  }

  it should "Rewrite (forall int x=0..3, y=0..5, z=0..2; xs[x+3*y+15*z]>0)" in correctNumbers(
    Seq((c(0), c(3)), (c(0), c(5)), (c(0), c(2))),
    {case Seq(x, y, z) => x + c(3)*y + c(15)*z},
    (xyz: Local[G]) => c(0) <= xyz && xyz < c(15)*c(2)
  )

  // This is not injective since x=0,y=4,z=0 is the same as x=1,y=0,z=1
  it should "Not rewrite (forall int x=0..3, y=0..5, z=0..2; xs[x+4*y+15*z]>0)" in notRewrite(
    Seq((c(0), c(3)), (c(0), c(5)), (c(0), c(2))),
    {case Seq(x, y, z) => x + c(4)*y + c(15)*z},
  )

  it should "Rewrite (forall int x=0..3, y=0..4, z=0..2; xs[x+3*y+15*z]>0) with (xyz%15/3 < 4) added" in correctNumbers(
    Seq((c(0), c(3)), (c(0), c(4)), (c(0), c(2))),
    {case Seq(x, y, z) => c(1)*x + c(3)*y + c(15)*z},
    (xyz: Local[G]) => ((xyz % c(15)) / c(3) < c(4)) && (c(0) <= xyz && xyz < c(15)*c(2))
  )

  it should "Rewrite (forall int x=0..2, y=0..5, z=0..2; xs[x+3*y+15*z]>0) with (xyz%15%3<2) added" in correctNumbers(
    Seq((c(0), c(2)), (c(0), c(5)), (c(0), c(2))),
    {case Seq(x, y, z) => c(1)*x + c(3)*y + c(15)*z},
    (xyz: Local[G]) => ((xyz % c(15)) % c(3) < c(2)) && (c(0) <= xyz && xyz < c(15)*c(2))
  )

  it should "Rewrite (forall int x=0..2, y=0..5, z=0..2; xs[-x+-3*y+-15*z]>0)" in correctNumbers(
    Seq((c(0), c(3)), (c(0), c(5)), (c(0), c(2))),
    {case Seq(x, y, z) => c(-1)*x + c(-3)*y + c(-15)*z},
    (xyz: Local[G]) => c(-15)*c(2) < xyz && xyz <= c(0)
  )

  // Should rewrite towards
  // (forall int i; i % 14 / 3 < 5 && 0 <= i && i < 14 * 2 && i % 14 % 3 + 3 * i % 14 / 3 < 14 ==> xs[i] > 0)
  // A simplifier could make this into
  // (forall int i; 0 <= i && i < 14 * 2 ==> xs[i] > 0)
  it should "Rewrite (forall int x=0..3, y=0..5, z=0..2; xs[x+3*y+14*z]>0)" in correctNumbers(
    Seq((c(0), c(3)), (c(0), c(5)), (c(0), c(2))),
    {case Seq(x, y, z) => x + c(3)*y + c(14)*z},
    (xyz: Local[G]) => xyz%c(14)/c(3)<c(5) && (c(0) <= xyz && xyz < c(14)*c(2)) &&
      xyz%c(14)%c(3)+c(3)*(xyz%c(14)/c(3))<c(14),
    Some({case Seq(x, y, z) => x + c(3)*y < c(14)})
  )

  it should "Rewrite (forall int x=0..n; xs[n-x-1]>0)" in correctNumbers(
    Seq((c(0), ns(0))),
    {case Seq(x) => ns(0) - x + c(-1)},
    (xyz: Local[G]) => c(-1) * ns(0) < (xyz - (c(-1) + ns(0))) && (xyz - (c(-1) + ns(0))) <= c(0)
  )

  // It has no proof that ns0 ns1 and ns0 are bigger than zero.
  it should "Not rewrite (forall x=0..nx, y=0..ny, z=0..nz;  xs[a0*x+a0*n0*y+a0*n0*n1*z]>0)" in notRewrite(
    Seq((c(0), ns(0)), (c(0), ns(1)), (c(0), ns(2))),
    {case Seq(x, y, z) => as(0)*x + as(0)*ns(1)*y + as(0)*ns(1)*ns(2)*z}
  )

  it should "Rewrite (forall x=0..nx, y=0..ny, z=0..nz;  xs[a0*x+a0*n0*y+a0*n0*n1*z]>0) given that a0!=0, nx>0, ny>0, nz>0" in correctNumbers(
    Seq((c(0), ns(0)), (c(0), ns(1)), (c(0), ns(2))),
    {case Seq(x,y,z) => as(0)*x + as(0)*ns(0)*y + as(0)*ns(0)*ns(1)*z},
    (xyz: Local[G]) => (c(0) <= xyz && xyz < as(0)*ns(0)*ns(1)*ns(2)) &&
      xyz % (as(0)*ns(0)*ns(1)) % (as(0)*ns(0)) % as(0) === c(0),
    None,
    Some(as(0) > c(0) && ns(0) > c(0) && ns(1) > c(0) && ns(2) > c(0))
  )

  it should "Rewrite (forall x=0..nx, y=0..ny, z=0..nz;  xs[a0*x+a1*y+a2*z]>0) given that a0>0, n0>0, n1>0, n2>0, a1=a0*n0, a2=a1*n1" in correctNumbers(
    Seq((c(0), ns(0)), (c(0), ns(1)), (c(0), ns(2))),
    {case Seq(x,y,z) => as(0)*x + as(1)*y + as(2)*z},
    (xyz: Local[G]) => (c(0) <= xyz && xyz < as(2)*ns(2)) &&
      xyz % as(2) % as(1) % as(0) === c(0),
    None,
    Some(as(0) > c(0) && ns(0) > c(0) && ns(1) > c(0) && ns(2) > c(0) && as(1)===as(0)*ns(0) && as(2)===as(1)*ns(1))
  )

  // since we have a1=a1*n0 this could lead to a stack overflow error when we keep replacing a1 with a1*n0 if we are
  // not careful
  it should "Not crash with stack overflow" in notRewrite(
    Seq((c(0), ns(0)), (c(0), ns(1)), (c(0), ns(2))),
    {case Seq(x,y,z) => as(0)*x + as(1)*y + as(2)*z},
    None,
    Some(as(0) > c(0) && ns(0) > c(0) && ns(1) > c(0) && ns(2) > c(0) && as(1)===as(1)*ns(0) && as(2)===as(1)*ns(1))
  )

  it should "Rewrite (forall x=xmin..xmin+nx, y=ymin..ymin+ny, z=zmin..zmin+nz;  xs[a0*x+a1*y+a2*z+b]>0) given that a0!=0, n0>0, n1>0, n2>0, a1=a0*n0, a2=a1*n1" in {
    val off = (as(9) + as(0)*xmin(0) + as(1)*xmin(1) + as(2)*xmin(2))
    correctNumbers(
      Seq((xmin(0), xmin(0) + ns(0)), (xmin(1), xmin(1) + ns(1)), (xmin(2), xmin(2) + ns(2))),
      { case Seq(x, y, z) => as(0) * x + as(1) * y + as(2) * z + as(9)},
      (xyz: Local[G]) =>
        (as(0) > c(0) ==> (c(0) <= xyz - off && xyz - off < as(2) * ns(2))) &&
          (as(0) < c(0) ==> (as(2) * ns(2) < xyz - off && xyz - off <= c(0))) &&
          abs(xyz - off) % as(2) % as(1) % as(0) === c(0),
      None,
      Some((as(0) !== c(0)) && ns(0) > c(0) && ns(1) > c(0) && ns(2) > c(0) && as(1) === as(0) * ns(0) && as(2) === as(1) * ns(1))
    )
  }
}
