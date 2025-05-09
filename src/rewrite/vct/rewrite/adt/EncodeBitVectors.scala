package vct.rewrite.adt

import hre.data.BitString
import vct.col.ast.{
  Asserting,
  BitAnd,
  BitNot,
  BitOr,
  BitShl,
  BitShr,
  BitUShr,
  BitXor,
  Declaration,
  Expr,
  Function,
  FunctionInvocation,
  Let,
  Minus,
  Node,
  PolarityDependent,
  Result,
  Select,
  SmtlibBitvecLiteral,
  SmtlibBv2Nat,
  SmtlibBvAnd,
  SmtlibBvNot,
  SmtlibBvOr,
  SmtlibBvShl,
  SmtlibBvShr,
  SmtlibInt2Bv,
  SuccessorsProvider,
  SuccessorsProviderTrafo,
  TBool,
  TInt,
  TSmtlibBitVector,
  UnitAccountedPredicate,
  Variable,
  Z3BvSLt,
  Z3BvSShr,
  Z3BvXor,
}
import vct.col.origin.{
  AbstractApplicable,
  AssertFailed,
  Blame,
  IntegerOutOfBounds,
  LabelContext,
  Origin,
  PanicBlame,
  TrueSatisfiable,
}
import vct.col.rewrite.{
  Generation,
  NonLatchingRewriter,
  Rewriter,
  RewriterBuilderArg,
}
import vct.col.util.AstBuildHelpers._

import scala.collection.mutable

case object EncodeBitVectors extends RewriterBuilderArg[Boolean] {
  private case class OutOfBoundsBlame(
      node: Node[_],
      blame: Blame[IntegerOutOfBounds],
  )(implicit val bits: Int)
      extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      blame.blame(IntegerOutOfBounds(node, bits))
  }

  private val BaseOrigin: Origin = Origin(
    Seq(LabelContext("Bit vector helpers"))
  )

  override def key: String = "encodeBitVectors"

  override def desc: String = "Encodes bit vector operations into SMT-LIB types"
}

case class EncodeBitVectors[Pre <: Generation](opaque: Boolean)
    extends Rewriter[Pre] {
  import EncodeBitVectors._

  private val isInboundsMap: mutable.HashMap[(Int, Boolean), Function[Post]] =
    mutable.HashMap()
  private val assumeInboundsMap
      : mutable.HashMap[(Int, Boolean), Function[Post]] = mutable.HashMap()

  private def to(
      e: Expr[Post],
      blame: Blame[IntegerOutOfBounds],
  )(implicit bits: Int, signed: Boolean): Expr[Post] = {
    val stripped = StripAsserting().dispatch(e)
    val inner = let(
      TInt(),
      e,
      { ex: Expr[Post] =>
        Asserting(ensureInRange(stripped), ex)(OutOfBoundsBlame(e, blame))(e.o)
      },
    )
    if (opaque) { inner }
    else { SmtlibInt2Bv(inner, bits)(e.o) }
  }

  private def from(
      e: Expr[Post],
      doAssume: Boolean,
  )(implicit bits: Int, signed: Boolean): Expr[Post] = {
    implicit val o: Origin = e.o
    if (opaque) {
      return if (doAssume)
        assumeInRange(e)
      else
        e
    }
    if (signed) {
      val res = { ex: Expr[Post] =>
        Select(
          Z3BvSLt(e, SmtlibBitvecLiteral(BitString("0".repeat(bits)))),
          SmtlibBv2Nat(ex) - const(BigInt(2).pow(bits)),
          SmtlibBv2Nat(ex),
        )
      }
      if (doAssume)
        assumeInRange(e, res)
      else
        res(StripAsserting().dispatch(e))
    } else {
      SmtlibBv2Nat(
        if (doAssume)
          assumeInRange(e)
        else
          e
      )
    }
  }

  private case class StripAsserting[G]() extends NonLatchingRewriter[G, G]() {
    case class SuccOrIdentity()
        extends SuccessorsProviderTrafo[G, G](allScopes) {
      override def postTransform[T <: Declaration[G]](
          pre: Declaration[G],
          post: Option[T],
      ): Option[T] = Some(post.getOrElse(pre.asInstanceOf[T]))
    }
    override def succProvider: SuccessorsProvider[G, G] = SuccOrIdentity()

    override def dispatch(e: Expr[G]): Expr[G] =
      e match {
        case Asserting(_, body) => dispatch(body)
        case _ => e.rewriteDefault()
      }
  }

  private def simplifyBV(e: Expr[Post])(implicit signed: Boolean) =
    e match {
      case SmtlibInt2Bv(
            Let(
              _,
              _,
              Asserting(
                _,
                Let(
                  _,
                  e0,
                  Asserting(
                    _,
                    Select(
                      Z3BvSLt(_, SmtlibBitvecLiteral(_)),
                      Minus(SmtlibBv2Nat(_), _),
                      SmtlibBv2Nat(_),
                    ),
                  ),
                ),
              ),
            ),
            _,
          ) if signed =>
        e0
      case Let(_, e0 @ FunctionInvocation(_, _, _, _, _, _), Asserting(_, _))
          if opaque &&
            e0.o.find[LabelContext].contains(BaseOrigin.get[LabelContext]) =>
        e0
      case SmtlibInt2Bv(Let(_, SmtlibBv2Nat(e0), Asserting(_, _)), _)
          if !signed =>
        e0
      case _ => e
    }

  private def ensureInRange(
      e: Expr[Post]
  )(implicit bits: Int, signed: Boolean): Expr[Post] = {
    functionInvocation[Post](
      TrueSatisfiable,
      isInboundsMap.getOrElseUpdate(
        (bits, signed), {
          implicit val o: Origin = BaseOrigin
          val (min, max): (Expr[Post], Expr[Post]) =
            if (signed) {
              (
                const(-BigInt(2).pow(bits - 1)),
                const(BigInt(2).pow(bits - 1) - 1),
              )
            } else { (const(0), const(BigInt(2).pow(bits) - 1)) }
          val x = new Variable[Post](TInt())(BaseOrigin.where(name = "x"))
          globalDeclarations.declare(withResult((result: Result[Post]) => {
            var ensures: Expr[Post] = result === (min <= x.get && x.get <= max)
            if (!opaque) {
              ensures =
                ensures &&
                  (result ===
                    (from(SmtlibInt2Bv(x.get, bits), doAssume = false) ===
                      x.get))
            }
            function(
              AbstractApplicable,
              TrueSatisfiable,
              TBool(),
              Seq(x),
              ensures = UnitAccountedPredicate(ensures),
            )(BaseOrigin.where(name = s"bv${bits}_is_inbounds"))
          }))
        },
      ).ref,
      args = Seq(e),
    )(e.o)
  }

  private def assumeInRange(
      e: Expr[Post],
      inner: Expr[Post] => Expr[Post] = { x => x },
  )(implicit bits: Int, signed: Boolean): Expr[Post] = {
    let(
      if (opaque)
        TInt()
      else
        TSmtlibBitVector(bits),
      e,
      { ex: Expr[Post] =>
        Asserting(
          functionInvocation[Post](
            TrueSatisfiable,
            assumeInboundsMap.getOrElseUpdate(
              (bits, signed), {
                implicit val o: Origin = BaseOrigin
                val x =
                  new Variable[Post](
                    if (opaque)
                      TInt()
                    else
                      TSmtlibBitVector(bits)
                  )(BaseOrigin.where(name = "x"))
                val (min, max): (Expr[Post], Expr[Post]) =
                  if (signed) {
                    (
                      const(-BigInt(2).pow(bits - 1)),
                      const(BigInt(2).pow(bits - 1) - 1),
                    )
                  } else { (const(0), const(BigInt(2).pow(bits) - 1)) }
                val ensures =
                  if (opaque) {
                    PolarityDependent(min <= x.get && x.get <= max, tt)
                  } else {
                    PolarityDependent(
                      SmtlibInt2Bv(from(x.get, doAssume = false), bits) ===
                        x.get,
                      tt,
                    )
                  }
                globalDeclarations.declare(
                  function[Post](
                    PanicBlame("Postcondition is assert true"),
                    TrueSatisfiable,
                    TBool(),
                    Seq(x),
                    body = Some(tt),
                    ensures = UnitAccountedPredicate(ensures),
                  )(BaseOrigin.where(name = s"bv${bits}_assume_inbounds"))
                )
              },
            ).ref,
            args = Seq(ex),
          )(e.o),
          inner(ex),
        )(TrueSatisfiable)(e.o)
      },
    )
  }

  private def binOp(
      op: (Expr[Post], Expr[Post]) => Expr[Post],
      l: Expr[Pre],
      r: Expr[Pre],
      b: Int,
      s: Boolean,
      blame: Blame[IntegerOutOfBounds],
  ): Expr[Post] = {
    implicit val bits: Int = b
    implicit val signed: Boolean = s
    from(
      op(
        simplifyBV(to(dispatch(l), blame)),
        simplifyBV(to(dispatch(r), blame)),
      ),
      doAssume = true,
    )
  }

  private def binOpFn(name: String): Function[Post] =
    globalDeclarations.declare(
      function[Post](
        AbstractApplicable,
        TrueSatisfiable,
        TInt(),
        args = Seq(
          new Variable(TInt())(BaseOrigin.where(name = "l")),
          new Variable(TInt())(BaseOrigin.where(name = "r")),
        ),
      )(BaseOrigin.where(name = name))
    )

  private lazy val and: (Expr[Post], Expr[Post], Origin) => Expr[Post] =
    if (opaque) {
      val f = binOpFn("bvand")
      (l, r, o) =>
        functionInvocation[Post](TrueSatisfiable, f.ref, args = Seq(l, r))(o)
    } else { (l, r, o) => SmtlibBvAnd[Post](l, r)(o) }
  private lazy val or: (Expr[Post], Expr[Post], Origin) => Expr[Post] =
    if (opaque) {
      val f = binOpFn("bvor")
      (l, r, o) =>
        functionInvocation[Post](TrueSatisfiable, f.ref, args = Seq(l, r))(o)
    } else { (l, r, o) => SmtlibBvOr[Post](l, r)(o) }
  private lazy val xor: (Expr[Post], Expr[Post], Origin) => Expr[Post] =
    if (opaque) {
      val f = binOpFn("bvxor")
      (l, r, o) =>
        functionInvocation[Post](TrueSatisfiable, f.ref, args = Seq(l, r))(o)
    } else { (l, r, o) => Z3BvXor[Post](l, r)(o) }
  private lazy val shl: (Expr[Post], Expr[Post], Origin) => Expr[Post] =
    if (opaque) {
      val f = binOpFn("bvshl")
      (l, r, o) =>
        functionInvocation[Post](TrueSatisfiable, f.ref, args = Seq(l, r))(o)
    } else { (l, r, o) => SmtlibBvShl[Post](l, r)(o) }
  private lazy val shr: (Expr[Post], Expr[Post], Origin) => Expr[Post] =
    if (opaque) {
      val f = binOpFn("bvshr")
      (l, r, o) =>
        functionInvocation[Post](TrueSatisfiable, f.ref, args = Seq(l, r))(o)
    } else { (l, r, o) => Z3BvSShr[Post](l, r)(o) }
  private lazy val ushr: (Expr[Post], Expr[Post], Origin) => Expr[Post] =
    if (opaque) {
      val f = binOpFn("bvushr")
      (l, r, o) =>
        functionInvocation[Post](TrueSatisfiable, f.ref, args = Seq(l, r))(o)
    } else { (l, r, o) => SmtlibBvShr[Post](l, r)(o) }
  private lazy val not: (Expr[Post], Origin) => Expr[Post] =
    if (opaque) {
      val f = globalDeclarations.declare(
        function[Post](
          AbstractApplicable,
          TrueSatisfiable,
          TInt(),
          args = Seq(new Variable(TInt())(BaseOrigin.where(name = "e"))),
        )(BaseOrigin.where(name = "bvnot"))
      )
      (e, o) =>
        functionInvocation[Post](TrueSatisfiable, f.ref, args = Seq(e))(o)
    } else { (e, o) => SmtlibBvNot[Post](e)(o) }

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o: Origin = e.o
    e match {
      case op @ BitAnd(l, r, b, s) => binOp(and(_, _, o), l, r, b, s, op.blame)
      case op @ BitOr(l, r, b, s) => binOp(or(_, _, o), l, r, b, s, op.blame)
      case op @ BitXor(l, r, b, s) => binOp(xor(_, _, o), l, r, b, s, op.blame)
      case op @ BitShl(l, r, b, s) => binOp(shl(_, _, o), l, r, b, s, op.blame)
      case op @ BitShr(l, r, b) =>
        binOp(shr(_, _, o), l, r, b, s = true, op.blame)
      case op @ BitUShr(l, r, b, s) =>
        binOp(ushr(_, _, o), l, r, b, s, op.blame)
      case op @ BitNot(arg, b, s) =>
        implicit val bits: Int = b
        implicit val signed: Boolean = s
        from(not(simplifyBV(to(dispatch(arg), op.blame)), o), doAssume = true)
      case _ => super.dispatch(e)
    }
  }
}
